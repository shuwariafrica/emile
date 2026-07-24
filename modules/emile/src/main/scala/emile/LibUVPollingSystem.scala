/*
 * Copyright 2025, 2026 Ali Rashid
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

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext

import cats.effect.IO
import cats.effect.unsafe.PollResult
import cats.effect.unsafe.PollingContext
import cats.effect.unsafe.PollingSystem
import cats.effect.unsafe.metrics.PollerMetrics

import emile.unsafe.LibUVPoller
import emile.unsafe.SignalSupervisor

/** The `cats.effect` [[cats.effect.unsafe.PollingSystem PollingSystem]] that plugs a libuv loop
  * into each work-stealing worker. Build it through [[LibUVPollingSystem$ LibUVPollingSystem]] and
  * install it with [[EmileIOApp]] or [[Emile.runtime]].
  */
final class LibUVPollingSystem private (config: LoopConfig, offloadParallelism: Int) extends PollingSystem:

  type Api = LibUVPollingSystem.Access
  type Poller = LibUVPoller

  // One signal supervisor per runtime, reached by workers through the Access handle
  // (LibUVPollingSystem.currentSupervisor), so a second runtime is never blocked by a first's dead one.
  private val signals: SignalSupervisor = new SignalSupervisor

  // The offload lane: a bounded fixed pool onto which the `offload` combinator shifts CPU-heavy work,
  // off the loop workers so they keep servicing I/O. Daemon threads, so the pool never holds the process
  // open. One lane per runtime, reached through the Access handle (LibUVPollingSystem.currentOffload).
  private val offloadThreads: ExecutorService =
    val counter = new AtomicInteger(0)
    Executors.newFixedThreadPool(
      offloadParallelism,
      (r: Runnable) =>
        val thread = new Thread(r, s"emile-offload-${counter.getAndIncrement()}")
        thread.setDaemon(true)
        thread
    )

  private val offloadLane: ExecutionContext = ExecutionContext.fromExecutor(offloadThreads)

  def makeApi(ctx: PollingContext[LibUVPoller]): LibUVPollingSystem.Access =
    new LibUVPollingSystem.Access(ctx, signals, offloadLane)

  def makePoller(): LibUVPoller =
    val p = new LibUVPoller(config)
    signals.electSupervisor(p)
    p

  def closePoller(p: LibUVPoller): Unit = p.close()

  def poll(p: LibUVPoller, nanos: Long): PollResult = p.poll(nanos)

  // One uv_run iteration drains every ready event, so there is no separate ready-event step.
  def processReadyEvents(p: LibUVPoller): Boolean = false

  def needsPoll(p: LibUVPoller): Boolean = p.needsPoll

  def interrupt(t: Thread, p: LibUVPoller): Unit = p.interrupt()

  // The work-stealing pool invokes this at runtime shutdown; an orderly shutdown lets any in-flight
  // offloaded compute finish while refusing new work.
  def close(): Unit = offloadThreads.shutdown()

  def metrics(p: LibUVPoller): PollerMetrics = p.metrics

end LibUVPollingSystem

/** Factory for [[LibUVPollingSystem]], and the worker-facing [[LibUVPollingSystem.Access Access]]
  * handle onto a [[emile.unsafe.LibUVPoller LibUVPoller]].
  */
object LibUVPollingSystem:

  /** A libuv polling system with the default [[LoopConfig]] and offload-lane parallelism. */
  def apply(): LibUVPollingSystem = apply(LoopConfig.default)

  /** A libuv polling system tuned by `config`, with the default offload-lane parallelism. */
  def apply(config: LoopConfig): LibUVPollingSystem = apply(config, defaultOffloadParallelism)

  /** A libuv polling system tuned by `config`, with an offload lane of `offloadParallelism`
    * threads.
    */
  def apply(config: LoopConfig, offloadParallelism: Int): LibUVPollingSystem =
    new LibUVPollingSystem(config, offloadParallelism)

  /** The default offload-lane parallelism - one thread per available processor. */
  def defaultOffloadParallelism: Int = java.lang.Runtime.getRuntime.availableProcessors()

  /** The worker-facing handle onto a [[emile.unsafe.LibUVPoller LibUVPoller]] - obtains the calling
    * worker's poller, its runtime's signal supervisor and offload lane, and tests poller ownership.
    */
  final class Access private[emile] (
    ctx: PollingContext[LibUVPoller],
    private[emile] val signals: SignalSupervisor,
    private[emile] val offload: ExecutionContext
  ):

    private[emile] def withCurrentPoller[A](f: LibUVPoller => IO[A]): IO[A] =
      IO.async_[LibUVPoller](cb => ctx.accessPoller(p => cb(Right(p)))).flatMap(f)

    private[emile] def isOwnPoller(p: LibUVPoller): Boolean = ctx.ownPoller(p)

  /** The libuv poller of the calling work-stealing worker - the entry point through which a
    * libuv-backed operation reaches its loop. Fails with
    * `EmileError.Runtime.MissingLibUVPollingSystem` when the runtime carries no libuv polling
    * system.
    */
  private[emile] def currentPoller: IO[LibUVPoller] =
    IO.pollers.flatMap: pollers =>
      pollers.collectFirst { case access: Access => access } match
        case Some(access) => access.withCurrentPoller(IO.pure)
        case None => IO.raiseError(EmileError.Runtime.MissingLibUVPollingSystem)

  /** The calling runtime's [[emile.unsafe.SignalSupervisor SignalSupervisor]] - the entry point
    * through which `Signal.watch` reaches the per-runtime supervisor. Fails with
    * `EmileError.Runtime.MissingLibUVPollingSystem` when the runtime carries no libuv polling
    * system.
    */
  private[emile] def currentSupervisor: IO[SignalSupervisor] =
    IO.pollers.flatMap: pollers =>
      pollers.collectFirst { case access: Access => access } match
        case Some(access) => IO.pure(access.signals)
        case None => IO.raiseError(EmileError.Runtime.MissingLibUVPollingSystem)

  /** The calling runtime's offload lane - the bounded compute pool through which the `offload`
    * combinator shifts CPU-heavy work off the loop workers. Fails with
    * `EmileError.Runtime.MissingLibUVPollingSystem` when the runtime carries no libuv polling
    * system.
    */
  private[emile] def currentOffload: IO[ExecutionContext] =
    IO.pollers.flatMap: pollers =>
      pollers.collectFirst { case access: Access => access } match
        case Some(access) => IO.pure(access.offload)
        case None => IO.raiseError(EmileError.Runtime.MissingLibUVPollingSystem)

end LibUVPollingSystem
