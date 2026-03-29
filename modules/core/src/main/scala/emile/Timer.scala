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

import scala.scalanative.libc.stdlib.calloc
import scala.scalanative.libc.stdlib.free
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.nullable.*

import emile.unsafe.CallbackStore
import emile.unsafe.LibUV

/** Timer handle for scheduling callbacks after a delay.
  *
  * Timers can be one-shot or repeating. A repeating timer will fire again after the repeat
  * interval, unless stopped.
  *
  * Timer callbacks are always invoked from the event loop thread.
  *
  * ==Phantom State Tracking==
  *
  * Timer uses phantom types to track handle state at compile time:
  *   - `Timer[Open]` - an active timer that can be started/stopped
  *   - `Timer[Closed]` - a closed timer (close initiated, memory pending free)
  *
  * This prevents operations on closed handles from compiling:
  * {{{
  * val timer: Timer[Open] = Timer.init(loop).toOption.get
  * val closed: Timer[Closed] = timer.closeSync.toOption.get
  * // closed.start(...) // Won't compile - start requires Timer[Open]
  * }}}
  *
  * ==Example==
  * {{{
  * // One-shot timer after 1 second
  * for
  *   timer <- Timer.init(loop)
  *   _ <- timer.start(Timeout.seconds(1), Timeout.Zero)(() => println("Fired!"))
  * yield timer
  *
  * // Repeating timer every 500ms
  * for
  *   timer <- Timer.init(loop)
  *   _ <- timer.start(Timeout.millis(500), Timeout.millis(500))(() => println("Tick"))
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

  /** Initialise a new timer handle.
    *
    * The timer must be started with `start` before it will fire. Returns a `Timer[Open]` indicating
    * the handle is ready for operations.
    *
    * @param loop The event loop to associate with this timer
    * @return Either an error or the initialised timer handle
    */
  def init(loop: Loop): Either[EmileError, Timer[Open]] =
    val size = LibUV.uv_handle_size(UV_TIMER)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
      val result = LibUV.uv_timer_init(loop.ptrUnsafe, handle)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(handle)
    }

  /** Create and start a one-shot timer.
    *
    * Convenience method combining init and start.
    *
    * @param loop The event loop
    * @param timeout Time until the callback fires
    * @param callback The callback to invoke
    * @return Either an error or the started timer
    */
  def after(loop: Loop, timeout: Timeout)(callback: () => Unit): Either[EmileError, Timer[Open]] =
    for
      timer <- init(loop)
      _ <- timer.start(timeout, Timeout.Zero)(callback)
    yield timer

  /** Create and start a repeating timer.
    *
    * Convenience method combining init and start.
    *
    * @param loop The event loop
    * @param interval The repeat interval (also used for initial timeout)
    * @param callback The callback to invoke on each tick
    * @return Either an error or the started timer
    */
  def interval(loop: Loop, interval: Timeout)(callback: () => Unit): Either[EmileError, Timer[Open]] =
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
    /** Start the timer.
      *
      * @param timeout Time until the first callback invocation (milliseconds)
      * @param repeat Repeat interval after the first callback (0 for one-shot)
      * @param callback The callback to invoke when the timer fires
      * @return Either an error or success
      */
    def start(timeout: Timeout, repeat: Timeout)(callback: () => Unit): Either[EmileError, Unit] =
      // Detach any existing callback to prevent leaks when restarting timer
      CallbackStore.detach(timer)

      // Store new callback in handle's data field
      CallbackStore.attach(timer, callback)

      val result = LibUV.uv_timer_start(
        timer,
        timerCallback,
        timeout.toMillis.toULong,
        repeat.toMillis.toULong
      )
      if result < 0 then
        // Clean up callback on failure
        CallbackStore.detach(timer)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())
    end start

    /** Start a one-shot timer (convenience method).
      *
      * @param timeout Time until the callback fires
      * @param callback The callback to invoke
      * @return Either an error or success
      */
    def startOnce(timeout: Timeout)(callback: () => Unit): Either[EmileError, Unit] =
      start(timeout, Timeout.Zero)(callback)

    /** Stop the timer.
      *
      * The callback will not be invoked after the timer is stopped. The timer can be restarted with
      * `start` or `again`.
      *
      * @return Either an error or success
      */
    def stop: Either[EmileError, Unit] =
      val result = LibUV.uv_timer_stop(timer)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        // Detach callback when timer stops
        CallbackStore.detach(timer)
        Right(())

    /** Restart a repeating timer.
      *
      * If the timer was started with a repeat interval, this will restart it using that interval.
      * If the repeat interval is zero, this has no effect.
      *
      * Note: If the timer has never been started, this will return an error.
      *
      * @return Either an error or success
      */
    def again: Either[EmileError, Unit] =
      val result = LibUV.uv_timer_again(timer)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Set the repeat interval.
      *
      * The timer does not need to be active for this to take effect. If the repeat value is set to
      * 0, the timer becomes one-shot.
      *
      * @param repeat The new repeat interval
      */
    def setRepeat(repeat: Timeout): Unit =
      LibUV.uv_timer_set_repeat(timer, repeat.toMillis.toULong)

    /** Get the current repeat interval.
      *
      * @return The repeat interval (0 for one-shot timers)
      */
    def repeatInterval: Timeout =
      Timeout.millis(LibUV.uv_timer_get_repeat(timer).toLong)

    /** Get the time until the timer fires.
      *
      * @return Timeout until the next callback, or 0 if expired/not started
      */
    def dueIn: Timeout =
      Timeout.millis(LibUV.uv_timer_get_due_in(timer).toLong)

    /** Close the timer synchronously (no callback).
      *
      * Transitions the timer to `Closed` state. The close is still asynchronous at the libuv level,
      * but no notification is provided.
      *
      * @return Either an error or the closed timer
      */

    /** Close the timer with a callback.
      *
      * The callback will be invoked when the close is complete. After calling this, the timer
      * should be considered closed.
      *
      * @param callback Callback invoked when close completes
      */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      Handle.closeAsyncWithDetach(timer, callback)
  end extension

  /** Timer callback that invokes the registered Scala callback. */
  private val timerCallback: LibUV.TimerCB = (handle: Ptr[Byte]) =>
    CallbackStore.get[() => Unit](handle).foreach { callback =>
      callback()
    }

end Timer
