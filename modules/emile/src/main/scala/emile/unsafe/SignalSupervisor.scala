/*
 * Copyright 2025, 2026 Ali Rashid
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package emile.unsafe

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.signal.sigaction
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*

import boilerplate.nullable.*
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue
import fs2.Stream

import emile.EmileError
import emile.ErrorCode
import emile.SignalNumber

/** A per-runtime signal supervisor: within one runtime, the first [[LibUVPoller]] built owns every
  * `uv_signal_t` and each delivery is broadcast to every subscriber's own queue. One instance lives
  * in each [[LibUVPollingSystem]], reached the way `currentPoller` reaches per-runtime state, so
  * sequential runtimes each get a fresh supervisor and concurrent runtimes each watch independently -
  * libuv delivers a process signal to every loop watching it. The signal-state map is confined to
  * the supervisor loop thread through [[Routing.onOwner]]; the lone `AtomicReference` only
  * publishes the elected supervisor to the workers that read it.
  */
final private[emile] class SignalSupervisor:
  import SignalSupervisor.*

  // Published once by electSupervisor; read by every worker.
  private val supervisor: AtomicReference[LibUVPoller | Null] = new AtomicReference(null) // scalafix:ok DisableSyntax.null

  // Supervisor-thread-confined: reached only inside Routing.onOwner(supervisorPoller).
  private val state: mutable.Map[SignalNumber, SignalState] = mutable.Map.empty

  /** Record this runtime's supervisor poller. Called once per poller by
    * `LibUVPollingSystem.makePoller`, which the pool constructor runs sequentially: the runtime's
    * first poller wins and the `compareAndSet` publishes it to its other workers.
    */
  def electSupervisor(p: LibUVPoller): Unit =
    supervisor.compareAndSet(null, p): Unit // scalafix:ok DisableSyntax.null

  /** Subscribe to `signum`. The internal currency is `IO`; `Signal.watch` lifts the result into the
    * public typed-error channel.
    */
  def subscribe(signum: SignalNumber): Stream[IO, Unit] =
    Stream.resource(register(signum)).flatMap(q => Stream.repeatEval(q.take))

  private def supervisorPoller: LibUVPoller =
    supervisor.get().getOrElse(throw EmileError.Runtime.MissingLibUVPollingSystem) // scalafix:ok DisableSyntax.throw

  private def register(signum: SignalNumber): Resource[IO, UnboundedQueue[IO, Unit]] =
    Resource.make(
      UnboundedQueue[IO, Unit].flatMap { queue =>
        Routing
          .onOwner(supervisorPoller) {
            val st = state.getOrElseUpdate(signum, newState())
            // Install before adding the subscriber: a failed start then leaves no dangling queue.
            if !st.installed then startSignal(signum, st)
            (st.subscribers += queue): Unit
          }
          .as(queue)
      }
    ) { queue =>
      Routing.onOwner(supervisorPoller) {
        state.get(signum).foreach { st =>
          (st.subscribers -= queue): Unit
          if st.subscribers.isEmpty && st.installed then stopSignal(signum, st)
        }
      }
    }

  // signal-supervisor FFI: handle / disposition allocation and reinterpretation, the null sigaction
  // argument, the install-flag var, and throws on a missing runtime or a signal-init failure.
  // scalafix:off DisableSyntax

  // Allocates a signum's reusable handle and disposition buffer and stores the state in the handle's
  // data slot, so the static signalCb recovers this runtime's subscribers from the handle libuv calls
  // back on. Frees the handle on an init failure - it is not yet init'd, so uv_close cannot reclaim it.
  private def newState(): SignalState =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_SIGNAL))
    if handle == null then throw new OutOfMemoryError("emile: uv_signal_t allocation failed")
    val initRc = LibUV.uv_signal_init(supervisorPoller.loop, handle)
    if initRc != 0 then
      stdlib.free(handle)
      throw EmileError.Runtime.System(ErrorCode(initRc))
    val saved = stdlib.calloc(1.toCSize, DispositionBytes.toCSize)
    if saved == null then
      LibUV.uv_close(handle, freeSignalHandleCb)
      throw new OutOfMemoryError("emile: sigaction allocation failed")
    val st = new SignalState(handle, saved)
    CallbackBridge.store(supervisorPoller, handle, st)
    st

  // uv_signal_stop restores only SIG_DFL, not the prior disposition, so it is captured here and
  // restored on stop. A capture failure cannot outlive a successful start (both reject the same
  // invalid signum), so its result is discarded and uv_signal_start is the validator.
  private def startSignal(signum: SignalNumber, st: SignalState): Unit =
    sigaction(SignalNumber.unwrap(signum), noAction, st.saved.asInstanceOf[Ptr[sigaction]]): Unit
    checked(LibUV.uv_signal_start(st.handle, signalCb, SignalNumber.unwrap(signum)))
    st.installed = true

  // Restores, after stopping, the disposition captured at start. A signal arriving in that gap takes
  // SIG_DFL; for a process-directed signal the window is inherent (other threads are not blocked), so
  // the restore is still closer to correct than libuv's stop-to-SIG_DFL.
  private def stopSignal(signum: SignalNumber, st: SignalState): Unit =
    LibUV.uv_signal_stop(st.handle): Unit
    sigaction(SignalNumber.unwrap(signum), st.saved.asInstanceOf[Ptr[sigaction]], noAction): Unit
    st.installed = false

  // scalafix:on DisableSyntax

end SignalSupervisor

/** The state and static C callback shared by every [[SignalSupervisor]]. [[SignalState]] lives
  * here, not in the class, so the `uv_signal_cb` can recover it from the handle's data slot while
  * capturing no instance - a `CFuncPtr` must.
  */
@scala.annotation.internal.sharable
private[emile] object SignalSupervisor:

  // Per-signum state, touched only on the supervisor loop thread, so its buffer and flag need no
  // synchronisation. The handle and saved-disposition buffer are allocated once and reused across
  // start/stop cycles; the handle is reclaimed by LibUVPoller.close()'s uv_walk sweep, the small
  // disposition buffer only at runtime exit.
  final private class SignalState(val handle: Ptr[Byte], val saved: Ptr[Byte]):
    val subscribers: mutable.ListBuffer[UnboundedQueue[IO, Unit]] = mutable.ListBuffer.empty
    var installed: Boolean = false // scalafix:ok DisableSyntax.var

  // scalafix:off DisableSyntax

  // A null Ptr[sigaction] for the unused act/oact argument of a one-directional sigaction call.
  private val noAction: Ptr[sigaction] = fromRawPtr(Intrinsics.castLongToRawPtr(0L))

  // scala-native models POSIX sigset_t as a single pointer, so its sigaction struct is far smaller
  // than the real Linux one (a 1024-bit sigset_t mask alone is 128 bytes); sizing the saved buffer by
  // it would let a sigaction() call write out of bounds. Hold the disposition in a buffer sized to
  // exceed the real struct on every supported platform, opaque - captured and restored whole.
  private inline val DispositionBytes = 256

  private def checked(rc: Int): Unit =
    if rc != 0 then throw EmileError.Runtime.System(ErrorCode(rc))

  // uv_signal_cb on the supervisor loop thread. A static val capturing no instance, as a CFuncPtr must:
  // it recovers the per-signum state from the handle's data slot - which carries this runtime's
  // subscribers - and broadcasts. libuv passes the handle it called back on, so no signum lookup is
  // needed.
  private val signalCb: LibUV.SignalCB = (handle: Ptr[Byte], _: Int) =>
    CallbackBridge.load[SignalState](handle).subscribers.foreach(_.unsafeOffer(()))

  private val freeSignalHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end SignalSupervisor
