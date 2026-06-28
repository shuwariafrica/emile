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
import emile.unsafe.LibuvPoller
import emile.unsafe.Routing

/** A readiness condition on a polled file descriptor. */
enum FdEvent derives CanEqual:
  case Readable, Writable, Disconnect, Prioritized

final private class FdPollState(val handle: Ptr[Byte], val poller: LibuvPoller, val eventMask: Int)

/** A file-descriptor readiness watcher, backed by a libuv `uv_poll_t`. Acquired through
  * [[FdPoll$ FdPoll]].
  */
opaque type FdPoll = FdPollState

/** Resource, the one-shot await, and equality for [[FdPoll]]. */
object FdPoll:

  /** A scoped `uv_poll_t` watching `fd` for `events`. The handle is created on - and closed back on -
    * the loop of the worker the resource is acquired on. The descriptor must stay open for the
    * resource's lifetime.
    */
  def resource(fd: Int, events: Set[FdEvent]): EmResource[EmileError.Io, FdPoll] =
    Resource.make[EffIO.Of[EmileError.Io], FdPoll](acquire(fd, events))(release)

  given CanEqual[FdPoll, FdPoll] = CanEqual.derived

  extension (poll: FdPoll)
    /** Completes with the readiness conditions for which the descriptor next becomes ready.
      * One-shot: polling is armed for this call and stopped when the events arrive or the effect is
      * cancelled.
      */
    def await: EmIO[EmileError.Io, Set[FdEvent]] =
      EffIO.attempt(startPoll(poll), EmileError.Io.Unexpected(_))

  private def acquire(fd: Int, events: Set[FdEvent]): EmIO[EmileError.Io, FdPoll] =
    EffIO.lift:
      for
        poller <- LibuvPollingSystem.currentPoller
        handle <- IO(allocHandle())
        result <- Routing.onOwner(poller)(install(poller, handle, fd, events))
      yield result

  private def release(poll: FdPoll): EmIO[EmileError.Io, Unit] =
    EffIO.liftF(Routing.closeHandle(poll.poller, poll.handle))

  private def install(
    poller: LibuvPoller,
    handle: Ptr[Byte],
    fd: Int,
    events: Set[FdEvent]
  ): Either[EmileError.Io, FdPoll] =
    val rc = LibUV.uv_poll_init(poller.loop, handle, fd)
    if rc != 0 then
      stdlib.free(handle)
      Left(IoMapping.fromCode(rc))
    else Right(new FdPollState(handle, poller, eventMask(events)))

  private def startPoll(poll: FdPoll): IO[Set[FdEvent]] =
    IO.async[Set[FdEvent]]: cb =>
      Routing.onOwner(poll.poller):
        CallbackBridge.store(poll.poller, poll.handle, new PollHolder(poll.poller, cb))
        val rc = LibUV.uv_poll_start(poll.handle, poll.eventMask, pollCb)
        if rc < 0 then
          CallbackBridge.clear(poll.poller, poll.handle)
          cb(Left(IoMapping.fromCode(rc)))
          None
        else Some(Routing.onOwner(poll.poller)(stopPoll(poll.poller, poll.handle)))

  // Both the one-shot trampoline and the cancellation finaliser run this; uv_poll_stop is idempotent.
  private def stopPoll(poller: LibuvPoller, handle: Ptr[Byte]): Unit =
    LibUV.uv_poll_stop(handle): Unit
    CallbackBridge.clear(poller, handle)

  // Carries the poller so the trampoline's stopPoll can clear the handle's anchor.
  final private class PollHolder(val poller: LibuvPoller, val cb: Either[Throwable, Set[FdEvent]] => Unit)

  private def eventBit(event: FdEvent): Int = event match
    case FdEvent.Readable => LibUV.UV_READABLE
    case FdEvent.Writable => LibUV.UV_WRITABLE
    case FdEvent.Disconnect => LibUV.UV_DISCONNECT
    case FdEvent.Prioritized => LibUV.UV_PRIORITIZED

  private def eventMask(events: Set[FdEvent]): Int =
    events.foldLeft(0)((mask, event) => mask | eventBit(event))

  private def decodeEvents(mask: Int): Set[FdEvent] =
    FdEvent.values.iterator.filter(event => (mask & eventBit(event)) != 0).toSet

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
    if status < 0 then holder.cb(Left(IoMapping.fromCode(status)))
    else holder.cb(Right(decodeEvents(events)))

end FdPoll
