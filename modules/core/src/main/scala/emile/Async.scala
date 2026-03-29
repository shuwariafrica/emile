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

import boilerplate.nullable.*

import _root_.emile.unsafe.CallbackStore
import _root_.emile.unsafe.LibUV

import emile.EmileError
import emile.ErrorCode
import emile.HandleState
import emile.HandleType
import emile.Loop
import emile.Open

/** Async handle for cross-thread event loop wakeup.
  *
  * Async handles allow any thread to wake up the event loop and invoke a callback on the loop
  * thread. This is the primary mechanism for thread-safe communication with the event loop.
  *
  * '''Important:''' Multiple `send` calls may be coalesced into a single callback invocation. The
  * async mechanism is not a queue - it's a notification.
  *
  * ==Phantom State Tracking==
  *
  * Async uses phantom types to track handle state at compile time:
  *   - `Async[Open]` - an active async handle that can send signals
  *   - `Async[Closed]` - a closed async handle (close initiated)
  *
  * ==Thread Safety==
  * The `send` method is the only thread-safe operation on async handles. All other handle
  * operations must be performed from the loop thread.
  *
  * ==Example==
  * {{{
  * // Create async handle on main thread
  * val async = Async.init(loop)(() => println("Woken up!"))
  *
  * // From another thread, wake up the loop
  * async.foreach(_.send)
  * }}}
  */
opaque type Async[S <: HandleState] = Ptr[Byte]

object Async:
  // Type alias for open async handles (most common case)
  type OpenAsync = Async[Open]

  given [S <: HandleState]: CanEqual[Async[S], Async[S]] = CanEqual.derived

  /** Provide Handle type class instance for Async (works on any state). */
  given [S <: HandleState]: Handle[Async[S]] = Handle.fromPtr[Async[S]](_.ptr)

  // Handle type constant for uv_handle_size
  // toLibuvInline provides compile-time elimination when used directly
  private val UV_ASYNC = HandleType.toLibuvInline(HandleType.Async)

  /** Initialise a new async handle with a callback.
    *
    * The callback will be invoked on the event loop thread whenever `send` is called from any
    * thread.
    *
    * Unlike other handle types, the callback is provided at initialization time because async
    * handles are always active once created. Returns an `Async[Open]` indicating the handle is
    * ready for operations.
    *
    * @param loop The event loop to associate with this async handle
    * @param callback The callback to invoke when the async is signaled
    * @return Either an error or the initialised async handle
    */
  def init(loop: Loop)(callback: () => Unit): Either[EmileError, Async[Open]] =
    val size = LibUV.uv_handle_size(UV_ASYNC)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
      // Initialize the handle first
      val result = LibUV.uv_async_init(loop.ptrUnsafe, handle, asyncCallback)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        CallbackStore.attach(handle, callback)
        Right(handle)
    }
  end init

  /** Internal constructor from raw pointer. */
  private[emile] inline def apply[S <: HandleState](p: Ptr[Byte]): Async[S] = p

  /** Extension methods available on any async handle regardless of state. */
  extension [S <: HandleState](async: Async[S])
    /** Get the raw pointer. */
    private[emile] inline def ptr: Ptr[Byte] = async

  /** Extension methods only available on open async handles. */
  extension (async: Async[Open])
    /** Send a signal to wake up the event loop.
      *
      * This is the only thread-safe operation on async handles. It can be called from any thread at
      * any time.
      *
      * Multiple sends may be coalesced - if `send` is called multiple times before the event loop
      * processes the signal, the callback may only be invoked once.
      *
      * This function is also async-signal-safe, meaning it can be called from signal handlers.
      *
      * @return Either an error or success
      */
    def send: Either[EmileError, Unit] =
      val result = LibUV.uv_async_send(async)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Close the async handle with a callback.
      *
      * The callback will be invoked when the close is complete. After calling this, the handle
      * should be considered closed.
      *
      * @param callback Callback invoked when close completes
      */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      Handle.closeAsyncWithDetach(async, callback)
  end extension

  /** Async callback that invokes the registered Scala callback. */
  private val asyncCallback: LibUV.AsyncCB = (handle: Ptr[Byte]) =>
    CallbackStore.get[() => Unit](handle).foreach { callback =>
      callback()
    }

end Async
