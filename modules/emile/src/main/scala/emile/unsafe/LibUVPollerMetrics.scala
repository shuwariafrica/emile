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
package emile.unsafe

import cats.effect.unsafe.metrics.PollerMetrics

import emile.ErrorCode

/** How a libuv operation ended, for [[LibUVPollerMetrics.connectSettled]] - connect learns its fate
  * from emile's control flow (a cancelled handle, a failed connect, or a completed one) rather than
  * a single status code, so its settlement is reported as one of these rather than a raw `Int`.
  */
private[emile] enum OpOutcome derives CanEqual:
  case Succeeded, Errored, Canceled

/** The per-poller I/O operation counters exposed to cats-effect as
  * [[cats.effect.unsafe.metrics.PollerMetrics PollerMetrics]], so a [[LibUVPoller]]'s socket and
  * stream activity surfaces in `IORuntimeMetrics` rather than reading as all-zero.
  *
  * Every counter is mutated only on the poller's loop thread - each emile operation is submitted
  * under `Routing.onOwner` and every libuv completion callback fires inside `uv_run`, both on that
  * one thread - and read, without synchronisation, by the metrics sampler on another; the resulting
  * staleness is the same benign race cats-effect's own pollers accept.
  *
  * Write and connect are durable in-flight requests, so both track their full lifecycle including
  * an outstanding count. Read and accept do not map onto a discrete submit-then-complete request -
  * a read is a persistent `uv_read_start` delivering repeatedly, and accept is pull-driven off the
  * connection backlog - so each is counted as it completes, with no durable outstanding count.
  * Threadpool operations (DNS resolution and file I/O) have no [[PollerMetrics]] category and are
  * not counted here.
  */
// Monitoring counters: single-writer loop-thread mutation, read racily by the sampler - the same
// unsynchronised-var shape as cats-effect's EpollSystem poller metrics.
// scalafix:off DisableSyntax
final private[emile] class LibUVPollerMetrics extends PollerMetrics:

  private var readSubmitted, readSucceeded, readErrored = 0L
  private var writeSubmitted, writeSucceeded, writeErrored, writeCanceled = 0L
  private var writeOutstanding = 0
  private var connectSubmitted, connectSucceeded, connectErrored, connectCanceled = 0L
  private var connectOutstanding = 0
  private var acceptSubmitted, acceptSucceeded, acceptErrored = 0L

  /** Record a completed read delivery - data or a clean end-of-stream is a success, a read error a
    * failure.
    */
  def readSettled(succeeded: Boolean): Unit =
    readSubmitted += 1
    if succeeded then readSucceeded += 1 else readErrored += 1

  /** Record a completed accept - a produced socket is a success, a failed `uv_accept` a failure. */
  def acceptSettled(succeeded: Boolean): Unit =
    acceptSubmitted += 1
    if succeeded then acceptSucceeded += 1 else acceptErrored += 1

  /** Record a `uv_write` accepted onto the loop. */
  def writeStarted(): Unit =
    writeSubmitted += 1
    writeOutstanding += 1

  /** Record a write callback firing; the libuv `status` classifies it (`UV_ECANCELED`, e.g. from an
    * abortive close discarding queued writes, is a cancellation rather than an error).
    */
  def writeSettled(status: Int): Unit =
    writeOutstanding -= 1
    if status >= 0 then writeSucceeded += 1
    else if status == ErrorCode.UV_ECANCELED then writeCanceled += 1
    else writeErrored += 1

  /** Record a `uv_tcp_connect` / `uv_pipe_connect2` accepted onto the loop. */
  def connectStarted(): Unit =
    connectSubmitted += 1
    connectOutstanding += 1

  /** Record a connect completing with the given [[OpOutcome]]. */
  def connectSettled(outcome: OpOutcome): Unit =
    connectOutstanding -= 1
    outcome match
      case OpOutcome.Succeeded => connectSucceeded += 1
      case OpOutcome.Errored => connectErrored += 1
      case OpOutcome.Canceled => connectCanceled += 1

  def operationsOutstandingCount(): Int = writeOutstanding + connectOutstanding
  def totalOperationsSubmittedCount(): Long = readSubmitted + writeSubmitted + connectSubmitted + acceptSubmitted
  def totalOperationsSucceededCount(): Long = readSucceeded + writeSucceeded + connectSucceeded + acceptSucceeded
  def totalOperationsErroredCount(): Long = readErrored + writeErrored + connectErrored + acceptErrored
  def totalOperationsCanceledCount(): Long = writeCanceled + connectCanceled

  def acceptOperationsOutstandingCount(): Int = 0
  def totalAcceptOperationsSubmittedCount(): Long = acceptSubmitted
  def totalAcceptOperationsSucceededCount(): Long = acceptSucceeded
  def totalAcceptOperationsErroredCount(): Long = acceptErrored
  def totalAcceptOperationsCanceledCount(): Long = 0L

  def connectOperationsOutstandingCount(): Int = connectOutstanding
  def totalConnectOperationsSubmittedCount(): Long = connectSubmitted
  def totalConnectOperationsSucceededCount(): Long = connectSucceeded
  def totalConnectOperationsErroredCount(): Long = connectErrored
  def totalConnectOperationsCanceledCount(): Long = connectCanceled

  def readOperationsOutstandingCount(): Int = 0
  def totalReadOperationsSubmittedCount(): Long = readSubmitted
  def totalReadOperationsSucceededCount(): Long = readSucceeded
  def totalReadOperationsErroredCount(): Long = readErrored
  def totalReadOperationsCanceledCount(): Long = 0L

  def writeOperationsOutstandingCount(): Int = writeOutstanding
  def totalWriteOperationsSubmittedCount(): Long = writeSubmitted
  def totalWriteOperationsSucceededCount(): Long = writeSucceeded
  def totalWriteOperationsErroredCount(): Long = writeErrored
  def totalWriteOperationsCanceledCount(): Long = writeCanceled

end LibUVPollerMetrics
// scalafix:on DisableSyntax
