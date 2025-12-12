/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.scalanative.unsafe.*
import scala.scalanative.libc.stdlib.{calloc, free}
import io.github.arashi01.emile.unsafe.{LibUV, CallbackRegistry, CallbackIdUtils}

/**
 * Poll handle for watching file descriptors for I/O events.
 *
 * Poll handles are used to watch file descriptors for readability,
 * writability and disconnection, similar to POSIX `poll(2)`.
 *
 * '''Primary use case:''' Integrating external libraries that need event loop
 * notifications about socket status changes (e.g., c-ares, libssh2).
 *
 * '''Warning:''' For general socket I/O, prefer [[Tcp]], [[Udp]], etc. which are
 * faster and more scalable, especially on Windows.
 *
 * == Phantom State Tracking ==
 *
 * Poll uses phantom types to track handle state at compile time:
 * - `Poll[Open]` - an active poll handle that can be started/stopped
 * - `Poll[Closed]` - a closed poll handle (close initiated)
 *
 * == Important Constraints ==
 *
 * - Do not have multiple active poll handles for the same file descriptor
 * - Do not close a file descriptor while it's being polled
 * - The fd can be safely closed immediately after `stop` or `close`
 * - On Windows, only sockets can be polled (not regular file descriptors)
 *
 * == Example ==
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

  /**
   * Initialize a poll handle for a file descriptor.
   *
   * The file descriptor will be set to non-blocking mode.
   * Returns a `Poll[Open]` indicating the handle is ready for operations.
   *
   * @param loop The event loop
   * @param fd The file descriptor to poll
   * @return Either an error or the initialized poll handle
   */
  def init(loop: Loop, fd: Int): Either[EmileError, Poll[Open]] =
    val size = LibUV.uv_handle_size(UV_POLL)
    val handle = calloc(1L, size.toLong)
    if handle == null then Left(EmileError.OutOfMemory)
    else
      val result = LibUV.uv_poll_init(loop.ptrUnsafe, handle, fd)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        Right(handle)

  /**
   * Initialize a poll handle for a socket.
   *
   * On Unix this is identical to `init`. On Windows this accepts
   * a SOCKET handle instead of a file descriptor.
   * Returns a `Poll[Open]` indicating the handle is ready for operations.
   *
   * @param loop The event loop  
   * @param socket The socket to poll (platform-specific type)
   * @return Either an error or the initialized poll handle
   */
  def initSocket(loop: Loop, socket: Ptr[Byte]): Either[EmileError, Poll[Open]] =
    val size = LibUV.uv_handle_size(UV_POLL)
    val handle = calloc(1L, size.toLong)
    if handle == null then Left(EmileError.OutOfMemory)
    else
      val result = LibUV.uv_poll_init_socket(loop.ptrUnsafe, handle, socket)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        Right(handle)

  /** Internal constructor from raw pointer. */
  private[emile] inline def apply[S <: HandleState](p: Ptr[Byte]): Poll[S] = p

  /** Extension methods available on any poll handle regardless of state. */
  extension [S <: HandleState](poll: Poll[S])
    /** Get the raw pointer. */
    private[emile] inline def ptr: Ptr[Byte] = poll

  /** Extension methods only available on open poll handles. */
  extension (poll: Poll[Open])
    /**
     * Start polling for the specified events.
     *
     * The callback will be invoked when any of the requested events are detected.
     * The callback receives:
     * - `status`: 0 on success, negative error code on failure
     * - `events`: Bitmask of detected events
     *
     * '''Note:''' Poll may occasionally signal readiness even when the fd isn't
     * actually ready. Always handle `EAGAIN` when reading/writing.
     *
     * @param events Events to watch for (use `|` to combine)
     * @param callback Callback invoked when events are detected
     * @return Either an error or success
     */
    def start(events: PollEvent*)(callback: (Int, Set[PollEvent]) => Unit): Either[EmileError, Unit] =
      // Unregister any existing callback to prevent leaks when restarting
      val existingId = CallbackIdUtils.getCallbackId(poll)
      if existingId != 0L then
        val _ = CallbackRegistry.unregister(existingId)

      // Register new callback
      val callbackId = CallbackRegistry.register(callback)
      CallbackIdUtils.setCallbackId(poll, callbackId)

      val eventMask = events.foldLeft(0)(_ | _.toLibuv)
      val result = LibUV.uv_poll_start(poll, eventMask, pollCallback)
      if result < 0 then
        val _ = CallbackRegistry.unregister(callbackId)
        CallbackIdUtils.clearCallbackId(poll)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        Right(())

    /**
     * Stop polling.
     *
     * The callback will no longer be invoked. The file descriptor can be
     * safely closed after this call returns.
     *
     * @return Either an error or success
     */
    def stop: Either[EmileError, Unit] =
      val result = LibUV.uv_poll_stop(poll)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        // Unregister callback when polling stops
        val callbackId = CallbackIdUtils.getCallbackId(poll)
        if callbackId != 0L then
          val _ = CallbackRegistry.unregister(callbackId)
          CallbackIdUtils.clearCallbackId(poll)
        Right(())

    /**
     * Close the poll handle synchronously (no callback).
     *
     * Transitions the poll handle to `Closed` state. The close is still
     * asynchronous at the libuv level, but no notification is provided.
     * The file descriptor can be safely closed after this call.
     *
     * @return Either an error or the closed handle
     */
    def closeSync: Either[EmileError, Poll[Closed]] =
      if LibUV.uv_is_closing(poll) != 0 then Left(EmileError.AlreadyClosed)
      else
        // Unregister any existing callback before closing
        val existingId = CallbackIdUtils.getCallbackId(poll)
        if existingId != 0L then
          val _ = CallbackRegistry.unregister(existingId)
          CallbackIdUtils.clearCallbackId(poll)

        LibUV.uv_close(poll, Handle.nullCloseCallback)
        Right(poll)

    /**
     * Close the poll handle with a callback.
     *
     * The callback will be invoked when the close is complete.
     * After calling this, the handle should be considered closed.
     *
     * @param callback Callback invoked when close completes
     */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      if LibUV.uv_is_closing(poll) != 0 then
        callback(Left(EmileError.AlreadyClosed))
      else
        // First, unregister any existing callback
        val existingId = CallbackIdUtils.getCallbackId(poll)
        if existingId != 0L then
          val _ = CallbackRegistry.unregister(existingId)

        // Now register the close callback and store its ID
        val callbackId = CallbackRegistry.register(callback)
        CallbackIdUtils.setCallbackId(poll, callbackId)
        LibUV.uv_close(poll, Handle.closeCallback)

  /** Poll callback that invokes the registered Scala callback. */
  private val pollCallback: LibUV.PollCB = (handle: Ptr[Byte], status: CInt, events: CInt) =>
    val callbackId = CallbackIdUtils.getCallbackId(handle)
    CallbackRegistry.findAs[(Int, Set[PollEvent]) => Unit](callbackId).foreach { callback =>
      val eventSet = PollEvent.fromLibuv(events)
      callback(status, eventSet)
    }

end Poll

/**
 * Poll event types for [[Poll]] handles.
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

object PollEvent:
  /** Convert to libuv event bitmask value. */
  extension (e: PollEvent)
    def toLibuv: Int = e match
      case Readable    => 1  // UV_READABLE
      case Writable    => 2  // UV_WRITABLE
      case Disconnect  => 4  // UV_DISCONNECT
      case Prioritized => 8  // UV_PRIORITIZED

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
