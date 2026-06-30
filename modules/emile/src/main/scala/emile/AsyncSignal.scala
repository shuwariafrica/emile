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
package emile

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue
import fs2.Stream

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.Routing

final private class AsyncSignalState(
  val handle: Ptr[Byte],
  val poller: LibUVPoller,
  val wakeups: UnboundedQueue[IO, Unit]
)

/** A thread-safe event-loop wake-up, backed by a libuv `uv_async_t`. The handle lives on the worker
  * that acquires the resource; [[AsyncSignal$ AsyncSignal]].fire wakes that loop from any thread.
  */
opaque type AsyncSignal = AsyncSignalState

/** Resource, accessors, and equality for [[AsyncSignal]]. */
object AsyncSignal:

  /** A scoped `uv_async_t`. The handle is created on - and closed back on - the loop of the worker
    * the resource is acquired on.
    */
  def resource: EmResource[EmileError.IO, AsyncSignal] =
    Resource.make[EffIO.Of[EmileError.IO], AsyncSignal](acquire)(release)

  given CanEqual[AsyncSignal, AsyncSignal] = CanEqual.derived

  extension (signal: AsyncSignal)
    /** Wakes the owning loop; thread-safe and callable from any thread, as `uv_async_send` is
      * libuv's cross-thread primitive.
      */
    def fire: EmIO[EmileError.IO, Unit] =
      EffIO.suspend(LibUV.uv_async_send(signal.handle): Unit)

    /** A stream of wake-ups. libuv coalesces `uv_async_send`, so N fires may surface as M (<= N)
      * elements - an edge-triggered signal, not a counter.
      */
    def fires: EmStream[EmileError.IO, Unit] =
      Stream.repeatEval(signal.wakeups.take).translate(EffIO.liftK)

  private def acquire: EmIO[EmileError.IO, AsyncSignal] =
    EffIO.lift:
      for
        poller <- LibUVPollingSystem.currentPoller
        wakeups <- UnboundedQueue[IO, Unit]
        handle <- IO(allocHandle())
        result <- Routing.onOwner(poller)(install(poller, handle, wakeups))
      yield result

  private def release(signal: AsyncSignal): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(Routing.closeHandle(signal.poller, signal.handle))

  private def install(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    wakeups: UnboundedQueue[IO, Unit]
  ): Either[EmileError.IO, AsyncSignal] =
    val rc = LibUV.uv_async_init(poller.loop, handle, asyncCb)
    if rc != 0 then
      stdlib.free(handle)
      Left(IOMapping.fromCode(rc))
    else
      val state = new AsyncSignalState(handle, poller, wakeups)
      CallbackBridge.store(poller, handle, state)
      Right(state)

  // uv_async_t allocation: a null calloc result is OOM, surfaced by a throw.
  // scalafix:off DisableSyntax
  private def allocHandle(): Ptr[Byte] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_ASYNC))
    if handle == null then throw new OutOfMemoryError("emile: uv_async_t allocation failed")
    else handle
  // scalafix:on DisableSyntax

  // uv_async_cb: offer a wake-up to the subscriber queue; runs on the loop thread.
  private val asyncCb: LibUV.AsyncCB = (handle: Ptr[Byte]) => CallbackBridge.load[AsyncSignalState](handle).wakeups.unsafeOffer(())

end AsyncSignal
