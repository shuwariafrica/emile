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

// scalafix:off DisableSyntax.null, DisableSyntax.var, DisableSyntax.asInstanceOf; libuv FFI event loop

import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.Intrinsics.castObjectToRawPtr
import scala.scalanative.runtime.Intrinsics.castRawPtrToObject
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.runtime.toRawPtr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import emile.unsafe.LibUV
import emile.unsafe.types.UvAsyncPtr
import emile.unsafe.types.UvTimerPtr

/** Result of a poll operation.
  *
  * Mirrors cats-effect's PollResult semantics for seamless integration, while also being usable by
  * ZIO and other effect systems.
  */
sealed abstract class PollResult extends Product with Serializable derives CanEqual

object PollResult:
  /** All available ready events were polled. */
  case object Complete extends PollResult

  /** No active handles/requests; loop is idle. */
  case object Idle extends PollResult

  /** Poll was interrupted or timed out before any events became ready. This includes:
    *   - Timeout expiration
    *   - Cross-thread interrupt via `interrupt()`
    *   - libuv's `uv_stop()` was called
    */
  case object Interrupted extends PollResult
end PollResult

/** libuv event loop poller for effect system integration.
  *
  * This is the core abstraction that bridges libuv's event loop model with effect system polling
  * abstractions. It provides:
  *
  *   - '''cats-effect integration''': Implements the semantics required by `PollingSystem` -
  *     `poll()`, `processReadyEvents()`, `needsPoll()`, `interrupt()`
  *   - '''ZIO integration''': Can be used with ZIO's `Executor` model by using `poll()` with
  *     timeout 0 for non-blocking work stealing, and integrating interrupt with ZIO's worker
  *     unparking
  *
  * ==libuv Semantics==
  *
  * The poller leverages libuv's three run modes:
  *
  *   - `UV_RUN_DEFAULT`: Runs until no more active handles (used for indefinite blocking)
  *   - `UV_RUN_ONCE`: Blocks for I/O once, processes callbacks (used for timed blocking)
  *   - `UV_RUN_NOWAIT`: Non-blocking poll (used for timeout=0 or yielding)
  *
  * The poll/processReadyEvents split maps to libuv as:
  *   - `poll()` runs `uv_run()` which both waits for AND processes events
  *   - `processReadyEvents()` is a no-op (libuv already processed them in poll)
  *
  * This is different from epoll/kqueue where you poll for readiness then process separately, but
  * matches how effect systems actually use the API.
  *
  * ==Thread Safety==
  *
  *   - `interrupt()`: Thread-safe (uses `uv_async_send`)
  *   - All other methods: Must be called from the owning thread
  *
  * ==Memory Ownership==
  *
  * The Poller owns:
  *   - The libuv loop (allocated via `stdlib.calloc`, freed on close)
  *   - An async handle for interrupts (lazily allocated)
  *
  * Call `close()` to release all resources.
  *
  * @note For cats-effect integration, all workers share one Poller via `LibuvPollingSystem`.
  */
trait Poller:
  /** The underlying libuv event loop. */
  def loop: Loop

  /** Poll for I/O events, blocking for up to the specified timeout.
    *
    * This method integrates with libuv's `uv_run()`:
    *
    *   - `nanos == -1`: Block indefinitely (`UV_RUN_DEFAULT`)
    *   - `nanos == 0`: Non-blocking poll (`UV_RUN_NOWAIT`)
    *   - `nanos > 0`: Block up to timeout, then return (`UV_RUN_ONCE` with timer)
    *
    * @param nanos Maximum time to block in nanoseconds, or -1 for infinite
    * @return Poll result indicating what happened
    */
  def poll(nanos: Long): PollResult

  /** Process ready events after a successful poll.
    *
    * For libuv, this is a no-op since `uv_run()` already processes callbacks. However, this method
    * exists for API compatibility with cats-effect's `PollingSystem` which separates polling from
    * processing.
    *
    * @return true if any events caused fiber rescheduling
    */
  def processReadyEvents(): Boolean

  /** Check if there are pending events that need polling.
    *
    * Maps to `uv_loop_alive()` - returns true if the loop has:
    *   - Active handles (TCP, timers, etc.)
    *   - Active requests (pending I/O operations)
    *
    * @return true if `poll()` should be called again
    */
  def needsPoll: Boolean

  /** Interrupt a blocking poll from another thread.
    *
    * This is '''thread-safe''' and uses libuv's `uv_async_send()` which:
    *   - Is async-signal-safe
    *   - May coalesce multiple calls
    *   - Wakes up the loop if it's blocked in `uv_run()`
    */
  def interrupt(): Unit

  /** Request the loop to stop on the next iteration.
    *
    * This mirrors libuv's `uv_stop` behaviour and is treated as an interruption in poll semantics.
    */
  def stop(): Unit

  /** Close the poller and release all resources.
    *
    * This:
    *   1. Closes the interrupt async handle (if allocated)
    *   2. Drains the loop (closes all handles)
    *   3. Frees the loop memory
    *
    * Must be called from the owning thread.
    */
  def close(): Unit
end Poller

object Poller:
  // Handle type constants for handle sizes
  private val UV_ASYNC = HandleType.toLibuvInline(HandleType.Async)
  private val UV_TIMER = HandleType.toLibuvInline(HandleType.Timer)

  // Timer callback for poll timeout - does nothing, just wakes up the loop
  private val pollTimeoutCallback: LibUV.TimerCB = (_: Ptr[Byte]) => ()

  /** Create a new Poller with default loop configuration. */
  def apply(): Either[EmileError, Poller] = apply(LoopConfig.empty)

  /** Create a new Poller with the specified loop configuration.
    *
    * Each Poller owns its own libuv loop. In multi-threaded scenarios, each worker thread should
    * have its own Poller.
    *
    * @param config Loop configuration options
    * @return Either an error or the initialised Poller
    */
  def apply(config: LoopConfig): Either[EmileError, Poller] =
    Loop.create(config).map(loop => new PollerImpl(loop))

  /** Private implementation.
    *
    * Design decisions:
    *
    *   1. '''Lazy async handle''': The interrupt async handle is only allocated on first
    *      `interrupt()` call. Many use cases don't need cross-thread interruption.
    *   2. '''Lazy timeout timer''': A persistent heap-allocated timer for poll timeouts. Created on
    *      first timed poll, reused thereafter. Essential for cats-effect integration where
    *      `IO.sleep` uses the internal scheduler. The timer ensures we return from poll after the
    *      requested timeout.
    *   3. '''Volatile interrupted flag''': Checked before/after poll to handle the case where
    *      interrupt() is called between checking and blocking.
    */
  final private class PollerImpl(val loop: Loop) extends Poller:
    // Async handle for cross-thread interrupt (lazily initialised)
    @volatile private var asyncHandle: UvAsyncPtr = UvAsyncPtr.Null

    // Timer handle for poll timeout (lazily initialised, unreferenced so it
    // doesn't keep the loop alive when idle)
    @volatile private var timeoutTimer: UvTimerPtr = UvTimerPtr.Null

    // Interrupt flag - set by interrupt(), cleared by poll()
    @volatile private var interrupted: Boolean = false

    // Stop flag - set by stop(), cleared by poll()
    @volatile private var stopRequested: Boolean = false

    // Closed flag
    @volatile private var closed: Boolean = false

    // Hotpath: poll loop is called on every cats-effect worker tick
    override def poll(nanos: Long): PollResult =
      if closed then PollResult.Interrupted
      else if interrupted then
        interrupted = false
        PollResult.Interrupted
      else if stopRequested then
        stopRequested = false
        PollResult.Interrupted
      else
        // Hotpath: select libuv run mode based on requested timeout
        val result = nanos match
          case -1L =>
            // UV_RUN_ONCE: block for one iteration, essential for cats-effect
            // integration - UV_RUN_DEFAULT would loop internally until all
            // handles close, preventing fiber rescheduling
            LibUV.uv_run(loop.ptrUnsafe, RunMode.Once.toLibuv)
          case 0L =>
            LibUV.uv_run(loop.ptrUnsafe, RunMode.NoWait.toLibuv)
          case timeout if timeout > 0L =>
            // Timed poll via libuv timer - essential because IO.sleep uses
            // cats-effect's internal scheduler, not libuv timers
            pollWithTimeout((timeout / 1_000_000L).max(1L))
          case _ =>
            LibUV.uv_run(loop.ptrUnsafe, RunMode.NoWait.toLibuv)

        if interrupted then
          interrupted = false
          PollResult.Interrupted
        else if stopRequested then
          stopRequested = false
          PollResult.Interrupted
        else if result != 0 then PollResult.Complete
        else PollResult.Idle
    end poll

    /** Poll with a timeout using a persistent libuv timer.
      *
      * Uses a lazily-allocated timer that's reused across polls. The timer is unref'd so it doesn't
      * keep the loop alive when idle. We ref it before starting and unref it after stopping to
      * ensure proper behaviour.
      */
    private def pollWithTimeout(millis: Long): Int =
      val timer = ensureTimeoutTimer()
      if timer == UvTimerPtr.Null then LibUV.uv_run(loop.ptrUnsafe, RunMode.Once.toLibuv)
      else
        val startResult = LibUV.uv_timer_start(timer.ptr, pollTimeoutCallback, millis.toULong, 0.toULong)
        if startResult < 0 then LibUV.uv_run(loop.ptrUnsafe, RunMode.Once.toLibuv)
        else
          val runResult = LibUV.uv_run(loop.ptrUnsafe, RunMode.Once.toLibuv)
          val _ = LibUV.uv_timer_stop(timer.ptr)
          runResult

    /** Lazily initialise the timeout timer.
      *
      * The timer is heap-allocated and lives for the lifetime of the poller. It's unref'd so it
      * doesn't keep the loop alive when idle.
      */
    private def ensureTimeoutTimer(): UvTimerPtr =
      var timer = timeoutTimer
      if timer == UvTimerPtr.Null && !closed then
        synchronized:
          timer = timeoutTimer
          if timer == UvTimerPtr.Null && !closed then
            timer = initTimeoutTimer()
            timeoutTimer = timer
      timer

    private def initTimeoutTimer(): UvTimerPtr =
      val size = LibUV.uv_handle_size(UV_TIMER)
      val ptr = stdlib.calloc(1L, size.toLong)
      // Hotpath: direct null check - calloc returns null on OOM in native allocator
      if ptr == null then UvTimerPtr.Null
      else
        val result = LibUV.uv_timer_init(loop.ptrUnsafe, ptr)
        if result < 0 then
          stdlib.free(ptr)
          UvTimerPtr.Null
        else
          LibUV.uv_unref(ptr)
          UvTimerPtr(ptr)
    end initTimeoutTimer

    override def processReadyEvents(): Boolean =
      // No-op: libuv processes callbacks during uv_run
      false

    override def needsPoll: Boolean =
      !closed && loop.isAlive

    override def interrupt(): Unit =
      if !closed then
        interrupted = true
        // Send async signal to wake a potentially blocked uv_run
        val handle = ensureAsyncHandle()
        if handle != UvAsyncPtr.Null then
          val _ = LibUV.uv_async_send(handle.ptr)

    override def stop(): Unit =
      if !closed then
        stopRequested = true
        LibUV.uv_stop(loop.ptrUnsafe)

    override def close(): Unit =
      if !closed then
        closed = true
        interrupted = false

        val handle = asyncHandle
        if handle != UvAsyncPtr.Null then
          asyncHandle = UvAsyncPtr.Null
          if LibUV.uv_is_closing(handle.ptr) == 0 then LibUV.uv_close(handle.ptr, closeCallback)

        val timer = timeoutTimer
        if timer != UvTimerPtr.Null then
          timeoutTimer = UvTimerPtr.Null
          if LibUV.uv_is_closing(timer.ptr) == 0 then LibUV.uv_close(timer.ptr, closeCallback)

        // Drain the loop - this closes all handles and frees memory
        val _ = loop.closeDrain

    /** Lazily initialise the async handle.
      *
      * Double-checked locking pattern for thread safety. The async handle is used to wake up the
      * loop from other threads.
      */
    private def ensureAsyncHandle(): UvAsyncPtr =
      var handle = asyncHandle
      if handle == UvAsyncPtr.Null && !closed then
        synchronized:
          handle = asyncHandle
          if handle == UvAsyncPtr.Null && !closed then
            handle = initAsyncHandle()
            asyncHandle = handle
      handle

    private def initAsyncHandle(): UvAsyncPtr =
      val size = LibUV.uv_handle_size(UV_ASYNC)
      val ptr = stdlib.calloc(1L, size.toLong)
      // Hotpath: direct null check - calloc returns null on OOM in native allocator
      if ptr == null then UvAsyncPtr.Null
      else
        val result = LibUV.uv_async_init(loop.ptrUnsafe, ptr, asyncCallback)
        if result < 0 then
          stdlib.free(ptr)
          UvAsyncPtr.Null
        else
          LibUV.uv_handle_set_data(ptr, fromRawPtr[Byte](castObjectToRawPtr(this)))
          LibUV.uv_unref(ptr)
          UvAsyncPtr(ptr)
    end initAsyncHandle

    /** Invoked on the loop thread when async wake fires. */
    private[Poller] def onAsyncWake(): Unit =
      // Mark as interrupted and request stop so uv_run returns promptly
      interrupted = true
      stopRequested = true
      LibUV.uv_stop(loop.ptrUnsafe)
  end PollerImpl

  // Async callback - retrieve PollerImpl from handle data and signal stop
  private val asyncCallback: LibUV.AsyncCB = (handle: Ptr[Byte]) =>
    val dataPtr = LibUV.uv_handle_get_data(handle)
    if dataPtr != null then
      val poller = castRawPtrToObject(toRawPtr(dataPtr)).asInstanceOf[PollerImpl]
      poller.onAsyncWake()

  // Close callback - frees the async handle memory
  private val closeCallback: LibUV.CloseCB = (handle: Ptr[Byte]) =>
    // Clear data to avoid pinning the PollerImpl
    LibUV.uv_handle_set_data(handle, null.asInstanceOf[Ptr[Byte]])
    stdlib.free(handle)
end Poller
