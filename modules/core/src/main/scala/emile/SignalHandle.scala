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

import emile.unsafe.CallbackStore
import emile.unsafe.LibUV

/** Signal handle for receiving OS signals via libuv's event loop.
  *
  * Signal handles provide cross-platform signal notification. When a signal is received, the
  * registered callback is invoked on the event loop thread during normal loop iteration.
  *
  * ==Phantom State Tracking==
  *
  * SignalHandle uses phantom types to track handle state at compile time:
  *   - `SignalHandle[Open]` - an active signal handle
  *   - `SignalHandle[Closed]` - a closed signal handle
  *
  * ==Platform Support==
  *
  * libuv provides cross-platform signal support with some limitations:
  *
  * '''Unix/Linux/macOS:''' Full POSIX signal support. All standard signals can be watched (except
  * SIGKILL and SIGSTOP which cannot be caught).
  *
  * '''Windows:''' Signal reception is emulated for a subset of signals:
  *   - `SIGINT` - Ctrl+C pressed
  *   - `SIGBREAK` - Ctrl+Break pressed
  *   - `SIGHUP` - Console window closed (program has ~10s to cleanup)
  *   - `SIGWINCH` - Console resized (with limitations)
  *
  * Other signals (SIGTERM, SIGUSR1, etc.) can have watchers created but will never be received on
  * Windows. Calls to `raise()` or `abort()` are also not detected.
  *
  * ==Example==
  * {{{
  * // Watch for SIGINT (Ctrl+C)
  * for
  *   signal <- SignalHandle.init(loop)
  *   _ <- signal.start(Signal.SIGINT)(() => println("Received SIGINT"))
  * yield signal
  *
  * // One-shot signal handler
  * SignalHandle.once(loop, Signal.SIGTERM)(() => println("Shutdown requested"))
  * }}}
  *
  * @see [[Signal]] for signal number constants
  */
opaque type SignalHandle[S <: HandleState] = Ptr[Byte]

object SignalHandle:
  // Type alias for open signal handles (most common case)
  type OpenSignal = SignalHandle[Open]

  given [S <: HandleState]: CanEqual[SignalHandle[S], SignalHandle[S]] = CanEqual.derived

  /** Provide Handle type class instance for SignalHandle (works on any state). */
  given [S <: HandleState]: Handle[SignalHandle[S]] = Handle.fromPtr[SignalHandle[S]](_.ptr)

  // Handle type constant for uv_handle_size
  private val UV_SIGNAL = HandleType.toLibuvInline(HandleType.Signal)

  /** Initialise a new signal handle.
    *
    * The signal handle must be started with `start` or `startOnce` before it will receive signals.
    * Returns a `SignalHandle[Open]` indicating the handle is ready for operations.
    *
    * @param loop The event loop to associate with this signal handle
    * @return Either an error or the initialised signal handle
    */
  def init(loop: Loop): Either[EmileError, SignalHandle[Open]] =
    val size = LibUV.uv_handle_size(UV_SIGNAL)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
      val result = LibUV.uv_signal_init(loop.ptrUnsafe, handle)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(handle)
    }

  /** Create and start a signal watcher.
    *
    * Convenience method combining init and start.
    *
    * @param loop The event loop
    * @param signum The signal number to watch
    * @param callback The callback to invoke when the signal is received
    * @return Either an error or the started signal handle
    */
  def watch(loop: Loop, signum: Int)(callback: () => Unit): Either[EmileError, SignalHandle[Open]] =
    for
      signal <- init(loop)
      _ <- signal.start(signum)(callback)
    yield signal

  /** Create and start a one-shot signal watcher.
    *
    * The signal handler is automatically stopped after the first signal. Convenience method
    * combining init and startOnce.
    *
    * @param loop The event loop
    * @param signum The signal number to watch
    * @param callback The callback to invoke when the signal is received
    * @return Either an error or the started signal handle
    */
  def once(loop: Loop, signum: Int)(callback: () => Unit): Either[EmileError, SignalHandle[Open]] =
    for
      signal <- init(loop)
      _ <- signal.startOnce(signum)(callback)
    yield signal

  /** Internal constructor from raw pointer. */
  private[emile] inline def apply[S <: HandleState](p: Ptr[Byte]): SignalHandle[S] = p

  /** Extension methods available on any signal handle regardless of state. */
  extension [S <: HandleState](signal: SignalHandle[S])
    /** Get the raw pointer. */
    private[emile] inline def ptr: Ptr[Byte] = signal

  /** Extension methods only available on open signal handles. */
  extension (signal: SignalHandle[Open])
    /** Start watching for a signal.
      *
      * The callback will be invoked each time the signal is received. Multiple signals may be
      * coalesced if they arrive faster than the event loop can process them.
      *
      * @param signum The signal number to watch
      * @param callback The callback to invoke when the signal is received
      * @return Either an error or success
      */
    def start(signum: Int)(callback: () => Unit): Either[EmileError, Unit] =
      CallbackStore.attach(signal, callback)
      val result = LibUV.uv_signal_start(signal, signalCallback, signum)
      if result < 0 then
        CallbackStore.detach(signal)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Start watching for a signal (one-shot).
      *
      * The signal handler is automatically reset after the signal is received. This is useful for
      * handling shutdown signals where you only want to respond once.
      *
      * @param signum The signal number to watch
      * @param callback The callback to invoke when the signal is received
      * @return Either an error or success
      */
    def startOnce(signum: Int)(callback: () => Unit): Either[EmileError, Unit] =
      CallbackStore.attach(signal, callback)
      val result = LibUV.uv_signal_start_oneshot(signal, signalCallback, signum)
      if result < 0 then
        CallbackStore.detach(signal)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Stop watching for signals.
      *
      * The callback will not be invoked after the signal watcher is stopped. The signal handle can
      * be restarted with `start` or `startOnce`.
      *
      * @return Either an error or success
      */
    def stop: Either[EmileError, Unit] =
      val result = LibUV.uv_signal_stop(signal)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        CallbackStore.detach(signal)
        Right(())

    /** Close the signal handle with a callback.
      *
      * Drains pending signals from libuv's self-pipe before stopping, preventing the race where
      * `uv_signal_stop` deregisters the sigaction handler while a signal is in flight, which would
      * restore SIG_DFL and terminate the process.
      *
      * @param callback Callback invoked when close completes
      */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      if LibUV.uv_is_closing(signal) != 0 then callback(Left(EmileError.AlreadyClosed))
      else
        // Drain any pending signals from libuv's self-pipe so they dispatch
        // to active handlers before we deregister the sigaction handler
        val loop = LibUV.uv_handle_get_loop(signal)
        val _ = LibUV.uv_run(loop, RunMode.NoWait.toLibuv)
        CallbackStore.detach(signal)
        CallbackStore.attach(signal, callback)
        LibUV.uv_close(signal, Handle.closeCallback)
  end extension

  /** Signal callback: recovers Scala closure from handle data field. */
  private val signalCallback: LibUV.SignalCB = (handle: Ptr[Byte], _: CInt) => CallbackStore.get[() => Unit](handle).foreach(_.apply())

end SignalHandle
