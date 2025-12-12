/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.{IO, Resource}
import cats.effect.unsafe.{PollingSystem, PollingContext, PollResult as CatsPollResult}
import cats.effect.unsafe.metrics.PollerMetrics
import io.github.arashi01.emile.{Loop, LoopConfig, PollResult}
import io.github.arashi01.emile.{Poller as EmilePoller}

/**
 * cats-effect PollingSystem implementation backed by libuv.
 *
 * This makes libuv THE event loop for the cats-effect runtime, eliminating
 * the paradigm mismatch between libuv's callback-based model and cats-effect's
 * polling abstraction.
 *
 * == Architecture ==
 *
 * Each cats-effect worker thread gets its own `Poller` (which owns a libuv loop).
 * The polling system:
 *
 * 1. Creates pollers on demand via `makePoller()`
 * 2. Polls for I/O via `poll()` which calls libuv's `uv_run()`
 * 3. Interrupts blocked polls via `interrupt()` using `uv_async_send()`
 * 4. Cleans up via `closePoller()` and `close()`
 *
 * == Usage ==
 *
 * {{{
 * import cats.effect.{IOApp, IO, ExitCode}
 * import io.github.arashi01.emile.cats.LibuvPollingSystem
 *
 * object MyApp extends IOApp:
 *   override protected def pollingSystem = LibuvPollingSystem
 *
 *   def run(args: List[String]): IO[ExitCode] = ...
 * }}}
 *
 * Or use the provided `EmileIOApp` trait:
 *
 * {{{
 * import io.github.arashi01.emile.cats.EmileIOApp
 *
 * object MyApp extends EmileIOApp:
 *   def run(args: List[String]): IO[ExitCode] = ...
 * }}}
 *
 * == Thread Model ==
 *
 * - Each worker thread has its own libuv loop (via `LibuvPoller`)
 * - Handles created on one loop CANNOT be used on another
 * - Use `EmileIOApp.withLoop` to access the current thread's loop
 */
object LibuvPollingSystem extends PollingSystem:
  /** The API exposed to IO effects - provides access to the libuv loop. */
  type Api = LoopAccess

  /** The per-thread poller wrapping a libuv loop. */
  type Poller = LibuvPoller

  // Configuration for loop creation - uses AtomicReference for thread safety
  private val loopConfigRef = new java.util.concurrent.atomic.AtomicReference[LoopConfig](LoopConfig.empty)

  /**
   * Configure the polling system with custom loop options.
   *
   * Must be called before the runtime starts (i.e., before `run` in IOApp).
   *
   * @param config Loop configuration to use for all worker threads
   */
  def configure(config: LoopConfig): Unit =
    loopConfigRef.set(config)

  // =========================================================================
  // PollingSystem implementation
  // =========================================================================

  override def close(): Unit = ()
    // Nothing to do at system level - pollers are closed individually

  override def makeApi(ctx: PollingContext[LibuvPoller]): LoopAccess =
    new LoopAccess(ctx)

  override def makePoller(): LibuvPoller =
    EmilePoller(loopConfigRef.get()) match
      case Right(p) => new LibuvPoller(p)
      case Left(e)  => throw new RuntimeException(s"Failed to create libuv poller: ${e.getMessage}")

  override def closePoller(poller: LibuvPoller): Unit =
    poller.underlying.close()

  override def poll(poller: LibuvPoller, nanos: Long): CatsPollResult =
    poller.underlying.poll(nanos) match
      case PollResult.Complete    => CatsPollResult.Complete
      case PollResult.Incomplete  => CatsPollResult.Incomplete
      case PollResult.Interrupted => CatsPollResult.Interrupted

  override def processReadyEvents(poller: LibuvPoller): Boolean =
    poller.underlying.processReadyEvents()

  override def needsPoll(poller: LibuvPoller): Boolean =
    poller.underlying.needsPoll

  override def interrupt(targetThread: Thread, targetPoller: LibuvPoller): Unit =
    targetPoller.underlying.interrupt()

  override def metrics(poller: LibuvPoller): PollerMetrics =
    poller.metrics

  // =========================================================================
  // Nested types
  // =========================================================================

  /**
   * API for accessing the libuv loop from IO effects.
   *
   * Provides safe access to the current worker thread's loop.
   * Similar to `FileDescriptorPoller` for epoll/kqueue, but for libuv.
   */
  final class LoopAccess private[LibuvPollingSystem] (ctx: PollingContext[LibuvPoller]):
    /**
     * Execute a callback with access to the current thread's libuv loop.
     *
     * The callback will be executed on a worker thread that owns the loop.
     * This is the safe way to create libuv handles from within IO effects.
     *
     * @param f Callback that receives the loop
     */
    def withLoop(f: Loop => Unit): Unit =
      ctx.accessPoller(poller => f(poller.loop))

    /**
     * Get the current thread's loop, wrapped in a Resource that manages lifecycle.
     *
     * This is the recommended way to get a loop for creating libuv handles.
     * The loop is NOT closed when the resource is released (it belongs to the runtime).
     *
     * @return Resource providing access to the loop
     */
    def loop: Resource[IO, Loop] =
      Resource.eval(IO.async_[Loop] { cb =>
        ctx.accessPoller(poller => cb(Right(poller.loop)))
      })

    /**
     * Check if the current thread owns the given poller.
     *
     * @param poller The poller to check
     * @return true if safe to interact with this poller
     */
    def ownsPoller(poller: LibuvPoller): Boolean =
      ctx.ownPoller(poller)

  object LoopAccess:
    /**
     * Find the LoopAccess API if this runtime uses LibuvPollingSystem.
     *
     * @return Some(LoopAccess) if available, None otherwise
     */
    def find: IO[Option[LoopAccess]] =
      IO.pollers.map(_.collectFirst { case access: LoopAccess => access })

    /**
     * Get the LoopAccess API, failing if this runtime doesn't use LibuvPollingSystem.
     *
     * @return LoopAccess for the current runtime
     * @throws RuntimeException if LibuvPollingSystem is not installed
     */
    def get: IO[LoopAccess] =
      find.flatMap {
        case Some(access) => IO.pure(access)
        case None => IO.raiseError(
          new RuntimeException("LibuvPollingSystem is not installed in this IORuntime. " +
            "Use EmileIOApp or IORuntimeBuilder.setPollingSystem(LibuvPollingSystem).")
        )
      }

  /**
   * Per-thread poller instance wrapping a libuv Poller.
   */
  final class LibuvPoller private[LibuvPollingSystem] (
      private[LibuvPollingSystem] val underlying: EmilePoller
  ):
    /** The libuv loop for this poller. */
    def loop: Loop = underlying.loop

    /** Metrics for this poller. */
    private[LibuvPollingSystem] val metrics: PollerMetrics = new PollerMetrics:
      // libuv doesn't track individual operation counts like io_uring does.
      // We provide stub implementations. For production use, we could add
      // tracking in emile-core's callback registry.
      override def operationsOutstandingCount(): Int = 0
      override def totalOperationsSubmittedCount(): Long = 0L
      override def totalOperationsSucceededCount(): Long = 0L
      override def totalOperationsErroredCount(): Long = 0L
      override def totalOperationsCanceledCount(): Long = 0L
      override def acceptOperationsOutstandingCount(): Int = 0
      override def totalAcceptOperationsSubmittedCount(): Long = 0L
      override def totalAcceptOperationsSucceededCount(): Long = 0L
      override def totalAcceptOperationsErroredCount(): Long = 0L
      override def totalAcceptOperationsCanceledCount(): Long = 0L
      override def connectOperationsOutstandingCount(): Int = 0
      override def totalConnectOperationsSubmittedCount(): Long = 0L
      override def totalConnectOperationsSucceededCount(): Long = 0L
      override def totalConnectOperationsErroredCount(): Long = 0L
      override def totalConnectOperationsCanceledCount(): Long = 0L
      override def readOperationsOutstandingCount(): Int = 0
      override def totalReadOperationsSubmittedCount(): Long = 0L
      override def totalReadOperationsSucceededCount(): Long = 0L
      override def totalReadOperationsErroredCount(): Long = 0L
      override def totalReadOperationsCanceledCount(): Long = 0L
      override def writeOperationsOutstandingCount(): Int = 0
      override def totalWriteOperationsSubmittedCount(): Long = 0L
      override def totalWriteOperationsSucceededCount(): Long = 0L
      override def totalWriteOperationsErroredCount(): Long = 0L
      override def totalWriteOperationsCanceledCount(): Long = 0L
end LibuvPollingSystem
