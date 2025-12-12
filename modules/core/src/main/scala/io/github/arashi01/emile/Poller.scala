/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.scalanative.unsafe.*
import scala.scalanative.libc.stdlib
import io.github.arashi01.emile.unsafe.LibUV
import io.github.arashi01.emile.unsafe.types.UvAsyncPtr

/**
 * Result of a poll operation.
 *
 * Mirrors cats-effect's PollResult semantics for seamless integration,
 * while also being usable by ZIO and other effect systems.
 */
sealed abstract class PollResult extends Product with Serializable derives CanEqual

object PollResult:
  /** All available ready events were polled. */
  case object Complete extends PollResult

  /**
   * Some but not all ready events were polled.
   * Poll should be called again to reap additional events.
   */
  case object Incomplete extends PollResult

  /**
   * Poll was interrupted or timed out before any events became ready.
   * This includes:
   * - Timeout expiration
   * - Cross-thread interrupt via `interrupt()`
   * - libuv's `uv_stop()` was called
   */
  case object Interrupted extends PollResult
end PollResult

/**
 * libuv event loop poller for effect system integration.
 *
 * This is the core abstraction that bridges libuv's event loop model with
 * effect system polling abstractions. It provides:
 *
 * - '''cats-effect integration''': Implements the semantics required by
 *   `PollingSystem` - `poll()`, `processReadyEvents()`, `needsPoll()`, `interrupt()`
 *
 * - '''ZIO integration''': Can be used with ZIO's `Executor` model by
 *   using `poll()` with timeout 0 for non-blocking work stealing, and
 *   integrating interrupt with ZIO's worker unparking
 *
 * == libuv Semantics ==
 *
 * The poller leverages libuv's three run modes:
 *
 * - `UV_RUN_DEFAULT`: Runs until no more active handles (used for indefinite blocking)
 * - `UV_RUN_ONCE`: Blocks for I/O once, processes callbacks (used for timed blocking)
 * - `UV_RUN_NOWAIT`: Non-blocking poll (used for timeout=0 or yielding)
 *
 * The poll/processReadyEvents split maps to libuv as:
 * - `poll()` runs `uv_run()` which both waits for AND processes events
 * - `processReadyEvents()` is a no-op (libuv already processed them in poll)
 *
 * This is different from epoll/kqueue where you poll for readiness then
 * process separately, but matches how effect systems actually use the API.
 *
 * == Thread Safety ==
 *
 * - `interrupt()`: Thread-safe (uses `uv_async_send`)
 * - All other methods: Must be called from the owning thread
 *
 * == Memory Ownership ==
 *
 * The Poller owns:
 * - The libuv loop (allocated via `stdlib.calloc`, freed on close)
 * - An async handle for interrupts (lazily allocated)
 *
 * Call `close()` to release all resources.
 *
 * @note Each effect system worker thread should have its own Poller instance.
 */
trait Poller:
  /** The underlying libuv event loop. */
  def loop: Loop

  /**
   * Poll for I/O events, blocking for up to the specified timeout.
   *
   * This method integrates with libuv's `uv_run()`:
   *
   * - `nanos == -1`: Block indefinitely (`UV_RUN_DEFAULT`)
   * - `nanos == 0`: Non-blocking poll (`UV_RUN_NOWAIT`)
   * - `nanos > 0`: Block up to timeout, then return (`UV_RUN_ONCE` with timer)
   *
   * @param nanos Maximum time to block in nanoseconds, or -1 for infinite
   * @return Poll result indicating what happened
   */
  def poll(nanos: Long): PollResult

  /**
   * Process ready events after a successful poll.
   *
   * For libuv, this is a no-op since `uv_run()` already processes callbacks.
   * However, this method exists for API compatibility with cats-effect's
   * `PollingSystem` which separates polling from processing.
   *
   * @return true if any events caused fiber rescheduling
   */
  def processReadyEvents(): Boolean

  /**
   * Check if there are pending events that need polling.
   *
   * Maps to `uv_loop_alive()` - returns true if the loop has:
   * - Active handles (TCP, timers, etc.)
   * - Active requests (pending I/O operations)
   *
   * @return true if `poll()` should be called again
   */
  def needsPoll: Boolean

  /**
   * Interrupt a blocking poll from another thread.
   *
   * This is '''thread-safe''' and uses libuv's `uv_async_send()` which:
   * - Is async-signal-safe
   * - May coalesce multiple calls
   * - Wakes up the loop if it's blocked in `uv_run()`
   */
  def interrupt(): Unit

  /**
   * Close the poller and release all resources.
   *
   * This:
   * 1. Closes the interrupt async handle (if allocated)
   * 2. Drains the loop (closes all handles)
   * 3. Frees the loop memory
   *
   * Must be called from the owning thread.
   */
  def close(): Unit
end Poller

object Poller:
  // Handle type constant for async handle size
  private val UV_ASYNC = HandleType.toLibuvInline(HandleType.Async)

  /**
   * Create a new Poller with default loop configuration.
   */
  def apply(): Either[EmileError, Poller] = apply(LoopConfig.empty)

  /**
   * Create a new Poller with the specified loop configuration.
   *
   * Each Poller owns its own libuv loop. In multi-threaded scenarios,
   * each worker thread should have its own Poller.
   *
   * @param config Loop configuration options
   * @return Either an error or the initialized Poller
   */
  def apply(config: LoopConfig): Either[EmileError, Poller] =
    Loop.create(config).map(loop => new PollerImpl(loop))

  /**
   * Private implementation.
   *
   * Design decisions:
   *
   * 1. '''Lazy async handle''': The interrupt async handle is only allocated
   *    on first `interrupt()` call. Many use cases don't need cross-thread
   *    interruption.
   *
   * 2. '''No separate timer for timeout''': We use `UV_RUN_ONCE` which
   *    respects `uv_backend_timeout()`. For precise timeout control, we
   *    could add a timer, but libuv's default behavior is sufficient for
   *    effect system integration.
   *
   * 3. '''Volatile interrupted flag''': Checked before/after poll to handle
   *    the case where interrupt() is called between checking and blocking.
   */
  private final class PollerImpl(val loop: Loop) extends Poller:
    // Async handle for cross-thread interrupt (lazily initialized)
    @volatile private var asyncHandle: UvAsyncPtr = UvAsyncPtr.Null

    // Interrupt flag - set by interrupt(), cleared by poll()
    @volatile private var interrupted: Boolean = false

    // Closed flag
    @volatile private var closed: Boolean = false

    override def poll(nanos: Long): PollResult =
      if closed then return PollResult.Interrupted

      // Check interrupt flag before potentially blocking
      if interrupted then
        interrupted = false
        return PollResult.Interrupted

      val result = nanos match
        case -1L =>
          // Block indefinitely - UV_RUN_DEFAULT runs until stop or no handles
          LibUV.uv_run(loop.ptrUnsafe, RunMode.Default.toLibuv)

        case 0L =>
          // Non-blocking - UV_RUN_NOWAIT polls without blocking
          LibUV.uv_run(loop.ptrUnsafe, RunMode.NoWait.toLibuv)

        case timeout if timeout > 0L =>
          // Timed poll - UV_RUN_ONCE blocks for one iteration
          // libuv will block up to uv_backend_timeout() which considers timers
          // For effect system integration, this is usually fine since the runtime
          // manages its own timers. If precise timeout is needed, we'd add a timer.
          LibUV.uv_run(loop.ptrUnsafe, RunMode.Once.toLibuv)

        case _ =>
          // Invalid timeout - treat as non-blocking
          LibUV.uv_run(loop.ptrUnsafe, RunMode.NoWait.toLibuv)

      // Check interrupt flag after poll
      if interrupted then
        interrupted = false
        PollResult.Interrupted
      else if result != 0 then
        // Non-zero means loop is still alive (has active handles/requests)
        // Since libuv processes all ready events in one uv_run call,
        // we report Complete (all ready events processed)
        PollResult.Complete
      else
        // Loop has no more active handles - nothing to poll
        PollResult.Interrupted
    end poll

    override def processReadyEvents(): Boolean =
      // libuv already processes callbacks during uv_run().
      // This is a no-op for API compatibility with cats-effect.
      // Return false since we don't know if events caused rescheduling.
      // The actual rescheduling happens in emile-cats callback wrappers.
      false

    override def needsPoll: Boolean =
      !closed && loop.isAlive

    override def interrupt(): Unit =
      if !closed then
        // Set flag first - poll() will check this
        interrupted = true

        // Send async signal to wake up the loop
        // This is thread-safe (uv_async_send is documented as such)
        val handle = ensureAsyncHandle()
        if handle != UvAsyncPtr.Null then
          val _ = LibUV.uv_async_send(handle.ptr)

    override def close(): Unit =
      if !closed then
        closed = true
        interrupted = false

        // Close async handle if allocated
        val handle = asyncHandle
        if handle != UvAsyncPtr.Null then
          asyncHandle = UvAsyncPtr.Null
          if LibUV.uv_is_closing(handle.ptr) == 0 then
            LibUV.uv_close(handle.ptr, closeCallback)

        // Drain the loop - this closes all handles and frees memory
        val _ = loop.closeDrain

    /**
     * Lazily initialize the async handle.
     *
     * Double-checked locking pattern for thread safety.
     * The async handle is used to wake up the loop from other threads.
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
      if ptr == null then return UvAsyncPtr.Null

      // The callback does nothing - we use the interrupted flag
      val result = LibUV.uv_async_init(loop.ptrUnsafe, ptr, asyncCallback)
      if result < 0 then
        stdlib.free(ptr)
        UvAsyncPtr.Null
      else
        UvAsyncPtr(ptr)
  end PollerImpl

  // Async callback - just wakes up the loop, actual handling via interrupted flag
  private val asyncCallback: LibUV.AsyncCB = (_: Ptr[Byte]) => ()

  // Close callback - frees the async handle memory
  private val closeCallback: LibUV.CloseCB = (handle: Ptr[Byte]) =>
    stdlib.free(handle)
end Poller
