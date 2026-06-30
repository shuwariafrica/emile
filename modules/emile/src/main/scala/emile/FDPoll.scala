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
import scala.scalanative.unsafe.CInt
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.Routing

/** A readiness condition on a polled file descriptor. */
enum FDEvent derives CanEqual:
  case Readable, Writable, Disconnect, Prioritized

final private class FDPollState(val handle: Ptr[Byte], val poller: LibUVPoller, val eventMask: Int)

/** A file-descriptor readiness watcher, backed by a libuv `uv_poll_t`. Acquired through
  * [[FDPoll$ FDPoll]].
  */
opaque type FDPoll = FDPollState

/** Resource, the one-shot await, and equality for [[FDPoll]]. */
object FDPoll:

  /** A scoped `uv_poll_t` watching `fd` for `events`. The handle is created on - and closed back on -
    * the loop of the worker the resource is acquired on. The descriptor must stay open for the
    * resource's lifetime.
    */
  def resource(fd: Int, events: Set[FDEvent]): EmResource[EmileError.IO, FDPoll] =
    Resource.make[EffIO.Of[EmileError.IO], FDPoll](acquire(fd, events))(release)

  given CanEqual[FDPoll, FDPoll] = CanEqual.derived

  extension (poll: FDPoll)
    /** Completes with the readiness conditions for which the descriptor next becomes ready.
      * One-shot: polling is armed for this call and stopped when the events arrive or the effect is
      * cancelled.
      */
    def await: EmIO[EmileError.IO, Set[FDEvent]] =
      EffIO.attempt(startPoll(poll), EmileError.IO.Unexpected(_))

  private def acquire(fd: Int, events: Set[FDEvent]): EmIO[EmileError.IO, FDPoll] =
    EffIO.lift:
      for
        poller <- LibUVPollingSystem.currentPoller
        handle <- IO(allocHandle())
        result <- Routing.onOwner(poller)(install(poller, handle, fd, events))
      yield result

  private def release(poll: FDPoll): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(Routing.closeHandle(poll.poller, poll.handle))

  private def install(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    fd: Int,
    events: Set[FDEvent]
  ): Either[EmileError.IO, FDPoll] =
    val rc = LibUV.uv_poll_init(poller.loop, handle, fd)
    if rc != 0 then
      stdlib.free(handle)
      Left(IOMapping.fromCode(rc))
    else Right(new FDPollState(handle, poller, eventMask(events)))

  private def startPoll(poll: FDPoll): IO[Set[FDEvent]] =
    IO.async[Set[FDEvent]]: cb =>
      Routing.onOwner(poll.poller):
        CallbackBridge.store(poll.poller, poll.handle, new PollHolder(poll.poller, cb))
        val rc = LibUV.uv_poll_start(poll.handle, poll.eventMask, pollCb)
        if rc < 0 then
          CallbackBridge.clear(poll.poller, poll.handle)
          cb(Left(IOMapping.fromCode(rc)))
          None
        else Some(Routing.onOwner(poll.poller)(stopPoll(poll.poller, poll.handle)))

  // Both the one-shot trampoline and the cancellation finaliser run this; uv_poll_stop is idempotent.
  private def stopPoll(poller: LibUVPoller, handle: Ptr[Byte]): Unit =
    LibUV.uv_poll_stop(handle): Unit
    CallbackBridge.clear(poller, handle)

  // Carries the poller so the trampoline's stopPoll can clear the handle's anchor.
  final private class PollHolder(val poller: LibUVPoller, val cb: Either[Throwable, Set[FDEvent]] => Unit)

  private def eventBit(event: FDEvent): Int = event match
    case FDEvent.Readable => LibUV.UV_READABLE
    case FDEvent.Writable => LibUV.UV_WRITABLE
    case FDEvent.Disconnect => LibUV.UV_DISCONNECT
    case FDEvent.Prioritized => LibUV.UV_PRIORITIZED

  private def eventMask(events: Set[FDEvent]): Int =
    events.foldLeft(0)((mask, event) => mask | eventBit(event))

  private def decodeEvents(mask: Int): Set[FDEvent] =
    FDEvent.values.iterator.filter(event => (mask & eventBit(event)) != 0).toSet

  // uv_poll_t allocation: a null calloc result is OOM, surfaced by a throw.
  // scalafix:off DisableSyntax
  private def allocHandle(): Ptr[Byte] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_POLL))
    if handle == null then throw new OutOfMemoryError("emile: uv_poll_t allocation failed")
    else handle
  // scalafix:on DisableSyntax

  // uv_poll_cb: deliver the first readiness event, then stop the one-shot poll.
  private val pollCb: LibUV.PollCB = (handle: Ptr[Byte], status: CInt, events: CInt) =>
    val holder = CallbackBridge.load[PollHolder](handle)
    stopPoll(holder.poller, handle)
    if status < 0 then holder.cb(Left(IOMapping.fromCode(status)))
    else holder.cb(Right(decodeEvents(events)))

end FDPoll
