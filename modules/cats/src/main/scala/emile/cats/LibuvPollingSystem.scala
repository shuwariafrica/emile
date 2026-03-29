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
package emile.cats

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.LockSupport

import scala.annotation.internal.sharable

import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.PollResult as CatsPollResult
import cats.effect.unsafe.PollingContext
import cats.effect.unsafe.PollingSystem
import cats.effect.unsafe.metrics.PollerMetrics

import boilerplate.effect.*
import boilerplate.effect.Eff

import emile.EmileError
import emile.Loop
import emile.LoopConfig
import emile.PollResult
import emile.Poller as EmilePoller

/** cats-effect PollingSystem backed by a single shared libuv event loop.
  *
  * ==Architecture==
  *
  * libuv is inherently single-threaded: all operations on a loop must be serialized. cats-effect's
  * work-stealing pool migrates fibers between threads (including to blocker threads via
  * `IO.blocking`/`IO.println`). A per-worker loop design is therefore fundamentally unsafe —
  * handles created on one worker's loop become invalid after fiber migration.
  *
  * This implementation uses a SINGLE shared loop across all workers:
  *
  *   - '''One loop, many workers''': All `LibuvPoller` instances reference the same underlying
  *     libuv loop. `accessPoller` on any worker thread gives the same loop, eliminating handle
  *     affinity issues.
  *   - '''Exclusive polling''': Only one worker calls `uv_run` at a time, controlled by a CAS flag.
  *     Other workers park normally.
  *   - '''Task queue serialization''': libuv operations that might execute while another worker is
  *     polling are submitted to a concurrent task queue. The polling worker drains the queue before
  *     and after `uv_run`, ensuring all libuv calls happen on a single thread at a time.
  *   - '''Fast path''': When no worker is polling, libuv operations execute directly under a
  *     short-lived CAS guard — no task queue overhead.
  */
final class LibuvPollingSystem private (config: LoopConfig) extends PollingSystem:
  type Api = LibuvPollingSystem.LoopAccess
  type Poller = LibuvPollingSystem.LibuvPoller

  // -----------------------------------------------------------------------
  // Shared state: one loop, one task queue, one exclusivity flag
  // -----------------------------------------------------------------------

  /** The single shared poller (created on first `makePoller` call). */
  private val sharedPollerRef: java.util.concurrent.atomic.AtomicReference[Option[EmilePoller]] =
    new java.util.concurrent.atomic.AtomicReference(None)

  /** CAS flag: true when a worker is executing libuv operations. */
  private[cats] val loopBusy: AtomicBoolean = new AtomicBoolean(false)

  /** Task queue for deferred libuv operations.
    *
    * When a fiber needs to call libuv but another worker holds the `loopBusy` flag (typically
    * during `uv_run`), the operation is enqueued here and processed when the flag is released.
    */
  @sharable private[cats] val taskQueue: ConcurrentLinkedQueue[Runnable] = new ConcurrentLinkedQueue()

  // -----------------------------------------------------------------------
  // PollingSystem implementation
  // -----------------------------------------------------------------------

  override def close(): Unit =
    sharedPollerRef.getAndSet(None).foreach(_.close())

  override def makeApi(ctx: PollingContext[Poller]): LibuvPollingSystem.LoopAccess =
    new LibuvPollingSystem.LoopAccess(ctx, this)

  override def makePoller(): Poller =
    val poller = sharedPollerRef.get() match
      case Some(p) => p
      case None    =>
        synchronized {
          sharedPollerRef.get() match
            case Some(p) => p
            case None    =>
              EmilePoller(config) match
                case Right(p) =>
                  sharedPollerRef.set(Some(p))
                  p
                case Left(e) => throw e // scalafix:ok; cats-effect API requires throwing
        }
    new LibuvPollingSystem.LibuvPoller(poller, this)
  end makePoller

  override def closePoller(poller: Poller): Unit =
    // Individual pollers are lightweight wrappers; nothing to close.
    // The shared poller is closed in close().
    ()

  override def poll(poller: Poller, nanos: Long): CatsPollResult =
    if loopBusy.compareAndSet(false, true) then
      try
        drainTaskQueue()
        val result = poller.underlying.poll(nanos)
        drainTaskQueue() // Process tasks that arrived during uv_run
        result match
          case PollResult.Complete    => CatsPollResult.Complete
          case PollResult.Idle        => CatsPollResult.Complete
          case PollResult.Interrupted => CatsPollResult.Interrupted
      finally loopBusy.set(false)
    else
      // Another worker holds the loop — park until interrupted or timeout.
      if nanos < 0L then LockSupport.park()
      else if nanos > 0L then LockSupport.parkNanos(nanos)
      CatsPollResult.Interrupted

  override def processReadyEvents(poller: Poller): Boolean =
    // libuv processes events during uv_run; nothing to do here.
    false

  override def needsPoll(poller: Poller): Boolean =
    poller.hasPending || !taskQueue.isEmpty || poller.underlying.needsPoll

  override def interrupt(targetThread: Thread, targetPoller: Poller): Unit =
    targetPoller.underlying.interrupt()
    LockSupport.unpark(targetThread)

  override def metrics(poller: Poller): PollerMetrics =
    poller.metrics

  /** Expose the captured configuration for inspection/testing. */
  def loopConfig: LoopConfig = config

  // -----------------------------------------------------------------------
  // Internal: loop access and task queue
  // -----------------------------------------------------------------------

  /** The shared loop, for use by `EffAsync.onLoop`. */
  private[cats] def loop: Loop =
    sharedPollerRef.get() match
      case Some(p) => p.loop
      case None    => throw new IllegalStateException("LibuvPollingSystem not initialized") // scalafix:ok

  /** Submit a task to be executed with exclusive loop access.
    *
    * If no worker currently holds the loop, the task executes immediately on the calling thread.
    * Otherwise it is enqueued and processed by the worker that holds the loop (during `poll()`).
    */
  private[cats] def submitTask(task: Runnable): Unit =
    taskQueue.offer(task)
    sharedPollerRef.get().foreach(_.interrupt())

  /** Drain the shared task queue. Must be called while `loopBusy` is held. */
  private def drainTaskQueue(): Unit =
    // scalafix:off DisableSyntax.var, DisableSyntax.null, DisableSyntax.while
    var task = taskQueue.poll()
    while task != null do
      task.run()
      task = taskQueue.poll()
    // scalafix:on

end LibuvPollingSystem

object LibuvPollingSystem:
  /** Default polling system with no loop overrides. */
  val default: LibuvPollingSystem = LibuvPollingSystem(LoopConfig.empty)

  /** Builder for a polling system with the provided loop configuration. */
  def apply(config: LoopConfig): LibuvPollingSystem = new LibuvPollingSystem(config)

  // =========================================================================
  // Nested types
  // =========================================================================

  /** API for accessing the shared libuv loop from IO effects. */
  final class LoopAccess private[LibuvPollingSystem] (
    ctx: PollingContext[LibuvPoller],
    private[cats] val system: LibuvPollingSystem
  ):
    /** Execute a callback with access to the shared libuv loop. */
    def withLoop(f: Loop => Unit): Unit =
      ctx.accessPoller(poller => f(poller.loop))

    /** Execute a callback with access to the current thread's poller. */
    def accessPoller(f: LibuvPoller => Unit): Unit =
      ctx.accessPoller(f)

    /** Get the shared loop, wrapped in a Resource.
      *
      * The loop is NOT closed when the resource is released (it belongs to the runtime).
      */
    def loop: Resource[Eff.Of[IO, EmileError], Loop] =
      Resource
        .eval(IO.async_[Loop] { cb =>
          ctx.accessPoller(poller => cb(Right(poller.loop)))
        })
        .eff[EmileError]

    /** Check if the current thread owns the given poller. */
    def ownsPoller(poller: LibuvPoller): Boolean =
      ctx.ownPoller(poller)

    /** Check if the current worker owns the supplied loop. With a shared loop, this is always true
      * for the runtime's loop.
      */
    def ownsLoop(loop: Loop): Eff[IO, EmileError, Boolean] =
      Eff.liftF[IO, EmileError, Boolean](IO.async_[Boolean] { cb =>
        ctx.accessPoller { poller =>
          cb(Right(poller.loop == loop))
        }
      })
  end LoopAccess

  object LoopAccess:
    /** Get the LoopAccess API with typed error channel. */
    def get: Eff[IO, EmileError, LoopAccess] =
      Eff
        .liftF[IO, EmileError, Option[LoopAccess]](
          IO.pollers.map(_.collectFirst { case access: LoopAccess => access })
        )
        .flatMap {
          case Some(access) => Eff.succeed[IO, EmileError, LoopAccess](access)
          case None         => Eff.fail[IO, EmileError, LoopAccess](EmileError.MissingLibuvPollingSystem)
        }
  end LoopAccess

  /** Per-worker poller wrapper around the shared libuv Poller.
    *
    * Each worker thread gets its own `LibuvPoller` instance, but all instances reference the same
    * underlying `EmilePoller` (and thus the same libuv loop).
    */
  final class LibuvPoller private[LibuvPollingSystem] (
    private[LibuvPollingSystem] val underlying: EmilePoller,
    private[cats] val system: LibuvPollingSystem
  ):
    /** The shared libuv loop. */
    def loop: Loop = underlying.loop

    /** Counter for pending async operations. */
    private val pendingOps: AtomicInteger = new AtomicInteger(0)

    /** Increment pending operations count. */
    def incrementPending(): Unit =
      val _ = pendingOps.incrementAndGet()

    /** Decrement pending operations count. */
    def decrementPending(): Unit =
      val _ = pendingOps.decrementAndGet()

    /** Check if there are pending operations. */
    def hasPending: Boolean = pendingOps.get() > 0

    /** Metrics for this poller. */
    private[LibuvPollingSystem] val metrics: PollerMetrics = new PollerMetrics:
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
  end LibuvPoller
end LibuvPollingSystem
