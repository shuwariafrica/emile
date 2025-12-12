/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.libc.stdlib.{calloc, free}
import io.github.arashi01.emile.unsafe.{LibUV, CallbackRegistry, CallbackIdUtils}

/**
 * Timer handle for scheduling callbacks after a delay.
 *
 * Timers can be one-shot or repeating. A repeating timer will fire
 * again after the repeat interval, unless stopped.
 *
 * Timer callbacks are always invoked from the event loop thread.
 *
 * == Phantom State Tracking ==
 *
 * Timer uses phantom types to track handle state at compile time:
 * - `Timer[Open]` - an active timer that can be started/stopped
 * - `Timer[Closed]` - a closed timer (close initiated, memory pending free)
 *
 * This prevents operations on closed handles from compiling:
 * {{{
 * val timer: Timer[Open] = Timer.init(loop).toOption.get
 * val closed: Timer[Closed] = timer.closeSync.toOption.get
 * // closed.start(...) // Won't compile - start requires Timer[Open]
 * }}}
 *
 * == Example ==
 * {{{
 * // One-shot timer after 1 second
 * for
 *   timer <- Timer.init(loop)
 *   _ <- timer.start(Duration.seconds(1), Duration.Zero)(() => println("Fired!"))
 * yield timer
 *
 * // Repeating timer every 500ms
 * for
 *   timer <- Timer.init(loop)
 *   _ <- timer.start(Duration.millis(500), Duration.millis(500))(() => println("Tick"))
 * yield timer
 * }}}
 */
opaque type Timer[S <: HandleState] = Ptr[Byte]

object Timer:
  // Type alias for open timers (most common case)
  type OpenTimer = Timer[Open]

  given [S <: HandleState]: CanEqual[Timer[S], Timer[S]] = CanEqual.derived

  /** Provide Handle type class instance for Timer (works on any state). */
  given [S <: HandleState]: Handle[Timer[S]] = Handle.fromPtr[Timer[S]](_.ptr)

  // Handle type constant for uv_handle_size
  // toLibuvInline provides compile-time elimination when used directly
  private val UV_TIMER = HandleType.toLibuvInline(HandleType.Timer)

  /**
   * Initialise a new timer handle.
   *
   * The timer must be started with `start` before it will fire.
   * Returns a `Timer[Open]` indicating the handle is ready for operations.
   *
   * @param loop The event loop to associate with this timer
   * @return Either an error or the initialised timer handle
   */
  def init(loop: Loop): Either[EmileError, Timer[Open]] =
    val size = LibUV.uv_handle_size(UV_TIMER)
    val handle = calloc(1L, size.toLong)
    if handle == null then Left(EmileError.OutOfMemory)
    else
      val result = LibUV.uv_timer_init(loop.ptrUnsafe, handle)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        Right(handle)

  /**
   * Create and start a one-shot timer.
   *
   * Convenience method combining init and start.
   *
   * @param loop The event loop
   * @param timeout Time until the callback fires
   * @param callback The callback to invoke
   * @return Either an error or the started timer
   */
  def after(loop: Loop, timeout: Duration)(callback: () => Unit): Either[EmileError, Timer[Open]] =
    for
      timer <- init(loop)
      _ <- timer.start(timeout, Duration.Zero)(callback)
    yield timer

  /**
   * Create and start a repeating timer.
   *
   * Convenience method combining init and start.
   *
   * @param loop The event loop
   * @param interval The repeat interval (also used for initial timeout)
   * @param callback The callback to invoke on each tick
   * @return Either an error or the started timer
   */
  def interval(loop: Loop, interval: Duration)(callback: () => Unit): Either[EmileError, Timer[Open]] =
    for
      timer <- init(loop)
      _ <- timer.start(interval, interval)(callback)
    yield timer

  /** Internal constructor from raw pointer. */
  private[emile] inline def apply[S <: HandleState](p: Ptr[Byte]): Timer[S] = p

  /** Extension methods available on any timer regardless of state. */
  extension [S <: HandleState](timer: Timer[S])
    /** Get the raw pointer. */
    private[emile] inline def ptr: Ptr[Byte] = timer

  /** Extension methods only available on open timers. */
  extension (timer: Timer[Open])
    /**
     * Start the timer.
     *
     * @param timeout Time until the first callback invocation (milliseconds)
     * @param repeat Repeat interval after the first callback (0 for one-shot)
     * @param callback The callback to invoke when the timer fires
     * @return Either an error or success
     */
    def start(timeout: Duration, repeat: Duration)(callback: () => Unit): Either[EmileError, Unit] =
      // Unregister any existing callback to prevent leaks when restarting timer
      val existingId = CallbackIdUtils.getCallbackId(timer)
      if existingId != 0L then
        val _ = CallbackRegistry.unregister(existingId)

      // Register new callback and store ID in timer data
      val callbackId = CallbackRegistry.register(callback)
      CallbackIdUtils.setCallbackId(timer, callbackId)

      val result = LibUV.uv_timer_start(
        timer,
        timerCallback,
        timeout.toMillis.toULong,
        repeat.toMillis.toULong
      )
      if result < 0 then
        // Clean up callback registration on failure
        val _ = CallbackRegistry.unregister(callbackId)
        CallbackIdUtils.clearCallbackId(timer)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        Right(())

    /**
     * Start a one-shot timer (convenience method).
     *
     * @param timeout Time until the callback fires
     * @param callback The callback to invoke
     * @return Either an error or success
     */
    def startOnce(timeout: Duration)(callback: () => Unit): Either[EmileError, Unit] =
      start(timeout, Duration.Zero)(callback)

    /**
     * Stop the timer.
     *
     * The callback will not be invoked after the timer is stopped.
     * The timer can be restarted with `start` or `again`.
     *
     * @return Either an error or success
     */
    def stop: Either[EmileError, Unit] =
      val result = LibUV.uv_timer_stop(timer)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        // Unregister callback when timer stops
        val callbackId = CallbackIdUtils.getCallbackId(timer)
        if callbackId != 0L then
          val _ = CallbackRegistry.unregister(callbackId)
          CallbackIdUtils.clearCallbackId(timer)
        Right(())

    /**
     * Restart a repeating timer.
     *
     * If the timer was started with a repeat interval, this will restart it
     * using that interval. If the repeat interval is zero, this has no effect.
     *
     * Note: If the timer has never been started, this will return an error.
     *
     * @return Either an error or success
     */
    def again: Either[EmileError, Unit] =
      val result = LibUV.uv_timer_again(timer)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /**
     * Set the repeat interval.
     *
     * The timer does not need to be active for this to take effect.
     * If the repeat value is set to 0, the timer becomes one-shot.
     *
     * @param repeat The new repeat interval
     */
    def setRepeat(repeat: Duration): Unit =
      LibUV.uv_timer_set_repeat(timer, repeat.toMillis.toULong)

    /**
     * Get the current repeat interval.
     *
     * @return The repeat interval (0 for one-shot timers)
     */
    def repeatInterval: Duration =
      Duration.millis(LibUV.uv_timer_get_repeat(timer).toLong)

    /**
     * Get the time until the timer fires.
     *
     * @return Duration until the next callback, or 0 if expired/not started
     */
    def dueIn: Duration =
      Duration.millis(LibUV.uv_timer_get_due_in(timer).toLong)

    /**
     * Close the timer synchronously (no callback).
     *
     * Transitions the timer to `Closed` state. The close is still
     * asynchronous at the libuv level, but no notification is provided.
     *
     * @return Either an error or the closed timer
     */
    def closeSync: Either[EmileError, Timer[Closed]] =
      if LibUV.uv_is_closing(timer) != 0 then Left(EmileError.AlreadyClosed)
      else
        // Unregister any existing callback before closing
        val existingId = CallbackIdUtils.getCallbackId(timer)
        if existingId != 0L then
          val _ = CallbackRegistry.unregister(existingId)
          CallbackIdUtils.clearCallbackId(timer)

        LibUV.uv_close(timer, Handle.nullCloseCallback)
        Right(timer)

    /**
     * Close the timer with a callback.
     *
     * The callback will be invoked when the close is complete.
     * After calling this, the timer should be considered closed.
     *
     * @param callback Callback invoked when close completes
     */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      if LibUV.uv_is_closing(timer) != 0 then
        callback(Left(EmileError.AlreadyClosed))
      else
        // First, unregister any existing callback
        val existingId = CallbackIdUtils.getCallbackId(timer)
        if existingId != 0L then
          val _ = CallbackRegistry.unregister(existingId)

        // Now register the close callback and store its ID
        val callbackId = CallbackRegistry.register(callback)
        CallbackIdUtils.setCallbackId(timer, callbackId)
        LibUV.uv_close(timer, Handle.closeCallback)

  /** Timer callback that invokes the registered Scala callback. */
  private val timerCallback: LibUV.TimerCB = (handle: Ptr[Byte]) =>
    val callbackId = CallbackIdUtils.getCallbackId(handle)
    CallbackRegistry.findAs[() => Unit](callbackId).foreach { callback =>
      callback()
    }

end Timer
