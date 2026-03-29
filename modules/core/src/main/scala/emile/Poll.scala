/*
 * Copyright 2025, 2026 Ali Rashid.
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

// scalafix:off DisableSyntax.var; libuv FFI poll handle

import scala.scalanative.libc.stdlib.calloc
import scala.scalanative.libc.stdlib.free
import scala.scalanative.unsafe.*

import boilerplate.nullable.*

import _root_.emile.unsafe.CallbackStore
import _root_.emile.unsafe.LibUV

/** writability and disconnection, similar to POSIX `poll(2)`.
  *
  * '''Primary use case:''' Integrating external libraries that need event loop notifications about
  * socket status changes (e.g., c-ares, libssh2).
  *
  * '''Warning:''' For general socket I/O, prefer [[Tcp]], [[Udp]], etc. which are faster and more
  * scalable, especially on Windows.
  *
  * ==Phantom State Tracking==
  *
  * Poll uses phantom types to track handle state at compile time:
  *   - `Poll[Open]` - an active poll handle that can be started/stopped
  *   - `Poll[Closed]` - a closed poll handle (close initiated)
  *
  * ==Important Constraints==
  *
  *   - Do not have multiple active poll handles for the same file descriptor
  *   - Do not close a file descriptor while it's being polled
  *   - The fd can be safely closed immediately after `stop` or `close`
  *   - On Windows, only sockets can be polled (not regular file descriptors)
  *
  * ==Example==
  * {{{
  * // Watch a file descriptor for readability
  * for
  *   poll <- Poll.init(loop, fd)
  *   _ <- poll.start(PollEvent.Readable) { (status, events) =>
  *     if status >= 0 && events.contains(PollEvent.Readable) then
  *       // fd is readable, but always handle EAGAIN
  *       readFromFd(fd)
  *   }
  * yield poll
  * }}}
  *
  * @see [[https://docs.libuv.org/en/stable/poll.html libuv Poll documentation]]
  */
opaque type Poll[S <: HandleState] = Ptr[Byte]

object Poll:
  // Type alias for open poll handles (most common case)
  type OpenPoll = Poll[Open]

  given [S <: HandleState]: CanEqual[Poll[S], Poll[S]] = CanEqual.derived

  /** Provide Handle type class instance for Poll (works on any state). */
  given [S <: HandleState]: Handle[Poll[S]] = Handle.fromPtr[Poll[S]](_.ptr)

  // Handle type constant for uv_handle_size
  // toLibuvInline provides compile-time elimination when used directly
  private val UV_POLL = HandleType.toLibuvInline(HandleType.Poll)

  /** Initialize a poll handle for a file descriptor.
    *
    * The file descriptor will be set to non-blocking mode. Returns a `Poll[Open]` indicating the
    * handle is ready for operations.
    *
    * @param loop The event loop
    * @param fd The file descriptor to poll
    * @return Either an error or the initialised poll handle
    */
  def init(loop: Loop, fd: Int): Either[EmileError, Poll[Open]] =
    val size = LibUV.uv_handle_size(UV_POLL)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
      val result = LibUV.uv_poll_init(loop.ptrUnsafe, handle, fd)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(handle)
    }

  /** Initialize a poll handle for a socket.
    *
    * On Unix this is identical to `init`. On Windows this accepts a SOCKET handle instead of a file
    * descriptor. Returns a `Poll[Open]` indicating the handle is ready for operations.
    *
    * @param loop The event loop
    * @param socket The socket to poll (platform-specific type)
    * @return Either an error or the initialised poll handle
    */
  def initSocket(loop: Loop, socket: Ptr[Byte]): Either[EmileError, Poll[Open]] =
    val size = LibUV.uv_handle_size(UV_POLL)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
      val result = LibUV.uv_poll_init_socket(loop.ptrUnsafe, handle, socket)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(handle)
    }

  /** Internal constructor from raw pointer. */
  private[emile] inline def apply[S <: HandleState](p: Ptr[Byte]): Poll[S] = p

  /** Extension methods available on any poll handle regardless of state. */
  extension [S <: HandleState](poll: Poll[S])
    /** Get the raw pointer. */
    private[emile] inline def ptr: Ptr[Byte] = poll

  /** Extension methods only available on open poll handles. */
  extension (poll: Poll[Open])
    /** Start polling for the specified events.
      *
      * The callback will be invoked when any of the requested events are detected. The callback
      * receives:
      *   - `status`: 0 on success, negative error code on failure
      *   - `events`: Bitmask of detected events
      *
      * '''Note:''' Poll may occasionally signal readiness even when the fd isn't actually ready.
      * Always handle `EAGAIN` when reading/writing.
      *
      * @param events Events to watch for (use `|` to combine)
      * @param callback Callback invoked when events are detected
      * @return Either an error or success
      */
    def start(events: PollEvent*)(callback: (Int, Set[PollEvent]) => Unit): Either[EmileError, Unit] =
      // Detach any existing callback to prevent leaks when restarting
      CallbackStore.detach(poll)

      // Store new callback in handle's data field
      CallbackStore.attach(poll, callback)

      val eventMask = events.foldLeft(0)(_ | _.toLibuv)
      val result = LibUV.uv_poll_start(poll, eventMask, pollCallback)
      if result < 0 then
        CallbackStore.detach(poll)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())
    end start

    /** Stop polling.
      *
      * The callback will no longer be invoked. The file descriptor can be safely closed after this
      * call returns.
      *
      * @return Either an error or success
      */
    def stop: Either[EmileError, Unit] =
      val result = LibUV.uv_poll_stop(poll)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        // Detach callback when polling stops
        CallbackStore.detach(poll)
        Right(())

    /** Close the poll handle with a callback.
      *
      * The callback will be invoked when the close is complete. After calling this, the handle
      * should be considered closed.
      *
      * @param callback Callback invoked when close completes
      */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      Handle.closeAsyncWithDetach(poll, callback)
  end extension

  /** Poll callback that invokes the registered Scala callback. */
  private val pollCallback: LibUV.PollCB = (handle: Ptr[Byte], status: CInt, events: CInt) =>
    CallbackStore.get[(Int, Set[PollEvent]) => Unit](handle).foreach { callback =>
      val eventSet = PollEvent.fromLibuv(events)
      callback(status, eventSet)
    }

end Poll

/** Poll event types for [[Poll]] handles.
  *
  * These correspond to libuv's `uv_poll_event` enum values.
  */
enum PollEvent derives CanEqual:
  /** File descriptor is readable. */
  case Readable

  /** File descriptor is writable. */
  case Writable

  /** Peer disconnected (optional, may not be reported on all platforms). */
  case Disconnect

  /** High priority data available (sysfs interrupts, TCP OOB). */
  case Prioritized
end PollEvent

object PollEvent:
  /** Convert to libuv event bitmask value. */
  extension (e: PollEvent)
    def toLibuv: Int = e match
      case Readable    => 1 // UV_READABLE
      case Writable    => 2 // UV_WRITABLE
      case Disconnect  => 4 // UV_DISCONNECT
      case Prioritized => 8 // UV_PRIORITIZED

  /** Convert libuv event bitmask to Set of PollEvent. */
  def fromLibuv(events: Int): Set[PollEvent] =
    var result = Set.empty[PollEvent]
    if (events & 1) != 0 then result += Readable
    if (events & 2) != 0 then result += Writable
    if (events & 4) != 0 then result += Disconnect
    if (events & 8) != 0 then result += Prioritized
    result

  /** Combine multiple events into a bitmask. */
  def combine(events: PollEvent*): Int =
    events.foldLeft(0)(_ | _.toLibuv)
end PollEvent
