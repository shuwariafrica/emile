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

/** The process-wide signal supervisor. One worker - the first [[LibuvPoller]] built - owns every
  * `uv_signal_t`, and each delivery is broadcast to every subscriber's own queue. All supervisor
  * mutable state is confined to the supervisor loop thread, reached through [[Routing.onOwner]];
  * the lone `AtomicReference` only publishes the elected supervisor to the workers that read it.
  */
// signal-supervisor FFI: install-flag var, null AtomicReference / election / handle sentinels,
// throws on a missing runtime and on signal-init failure.
// scalafix:off DisableSyntax
@scala.annotation.internal.sharable
private[emile] object SignalSupervisor:

  // Per-signum state, touched only on the supervisor loop thread, so its buffer and flag need no
  // synchronisation.
  final private class SignalState:
    val subscribers: mutable.ListBuffer[UnboundedQueue[IO, Unit]] = mutable.ListBuffer.empty
    var installed: Boolean = false

  // Supervisor-thread-confined: no synchronisation needed.
  private val state: mutable.Map[SignalNumber, SignalState] = mutable.Map.empty

  // Cross-thread-published: written once by makePoller(), read by every worker.
  private val supervisor: AtomicReference[LibuvPoller | Null] = new AtomicReference(null)

  /** Record the supervisor poller. Called once per poller by `LibuvPollingSystem.makePoller()`,
    * which the `WorkStealingThreadPool` constructor runs sequentially: the first poller wins, and
    * the `compareAndSet` publishes it to the other workers.
    */
  def electSupervisor(p: LibuvPoller): Unit =
    supervisor.compareAndSet(null, p): Unit

  private def supervisorPoller: LibuvPoller =
    supervisor.get().getOrElse(throw EmileError.Runtime.MissingLibuvPollingSystem)

  private def checked(rc: Int): Unit =
    if rc != 0 then throw EmileError.Runtime.System(ErrorCode(rc))

  // Runs on the supervisor loop thread. The handle is never uninstalled - an idle uv_signal_t costs
  // nothing, and LibuvPoller.close()'s uv_walk sweep frees it at shutdown.
  private def installHandle(signum: SignalNumber): Unit =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_SIGNAL))
    if handle == null then throw new OutOfMemoryError("emile: uv_signal_t allocation failed")
    checked(LibUV.uv_signal_init(supervisorPoller.loop, handle))
    checked(LibUV.uv_signal_start(handle, signalCb, SignalNumber.unwrap(signum)))

  /** Subscribe to `signum`. The internal currency is `IO`; `Signal.watch` lifts the result into the
    * public typed-error channel.
    */
  def subscribe(signum: SignalNumber): Stream[IO, Unit] =
    Stream.resource(register(signum)).flatMap(q => Stream.repeatEval(q.take))

  private def register(signum: SignalNumber): Resource[IO, UnboundedQueue[IO, Unit]] =
    Resource.make(
      UnboundedQueue[IO, Unit].flatMap { queue =>
        Routing
          .onOwner(supervisorPoller) {
            val st = state.getOrElseUpdate(signum, new SignalState)
            (st.subscribers += queue): Unit
            if !st.installed then
              installHandle(signum)
              st.installed = true
          }
          .as(queue)
      }
    ) { queue =>
      Routing.onOwner(supervisorPoller) {
        state.get(signum).foreach(st => (st.subscribers -= queue): Unit)
      }
    }

  // uv_signal_cb on the supervisor loop thread. libuv passes the signum directly, so no handle->data
  // recovery is needed; wrap is the unchecked constructor, as libuv guarantees a valid signum and a
  // validating one must never throw across the C ABI.
  private val signalCb: LibUV.SignalCB = (_: Ptr[Byte], signum: Int) =>
    state.get(SignalNumber.wrap(signum)) match
      case Some(st) => st.subscribers.foreach(_.unsafeOffer(()))
      case None => ()

end SignalSupervisor
// scalafix:on DisableSyntax
