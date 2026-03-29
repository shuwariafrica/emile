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

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Queue
import cats.effect.std.unsafe.UnboundedQueue

import boilerplate.effect.Eff

import emile.EmileError
import emile.Open
import emile.SignalHandle
import emile.SignalWatcher

/** cats-effect integration for signal watching.
  *
  * All libuv operations are serialized through `EffAsync.onLoop` / `EffAsync.closeHandle`, ensuring
  * thread safety with the shared loop.
  */
object SignalStream:

  /** Create a resource for watching signals with a queue-based notification.
    *
    * The queue will have units offered each time the signal is received. Use `queue.take` to await
    * signals.
    *
    * The returned tuple includes an `IO[Unit]` that completes when the signal handler is installed.
    * Use this to synchronise before sending signals.
    *
    * @param signum The signal number to watch
    * @return A resource providing (queue, ready) where ready signals handler installation
    */
  def watch(signum: Int): Resource[Eff.Of[IO, EmileError], (Queue[IO, Unit], IO[Unit])] =
    Resource
      .make(
        acquire = createWatcher(signum)
      )(
        release = { case (handle, _, _) => releaseWatcher(handle) }
      )
      .map { case (_, queue, ready) => (queue, ready) }

  private def createWatcher(
    signum: Int
  ): Eff[IO, EmileError, (SignalHandle[Open], UnboundedQueue[IO, Unit], IO[Unit])] =
    for
      // Create unbounded queue with unsafeOffer support
      queue <- Eff.liftF[IO, EmileError, UnboundedQueue[IO, Unit]](Queue.unsafeUnbounded[IO, Unit])
      // Create readiness gate - completes when handler is installed
      ready <- Eff.liftF[IO, EmileError, Deferred[IO, Unit]](Deferred[IO, Unit])
      // Start signal watcher via serialized loop access
      handle <- EffAsync.onLoop { loop =>
                  SignalWatcher.watch(loop)(signum) { () =>
                    queue.unsafeOffer(())
                  }
                }
      // Signal that handler is now installed
      _ <- Eff.liftF[IO, EmileError, Boolean](ready.complete(()))
    yield (handle, queue, ready.get)

  private def releaseWatcher(handle: SignalHandle[Open]): Eff[IO, EmileError, Unit] =
    EffAsync.closeHandle(handle.closeAsync)

  /** Await a single signal occurrence.
    *
    * Creates a watcher, waits for the signal to arrive, then cleans up.
    *
    * @param signum The signal number to wait for
    * @return Eff that completes when the signal is received
    */
  def awaitOnce(signum: Int): Eff[IO, EmileError, Unit] =
    // Use the watch resource with a single take - this keeps the loop resource open
    watch(signum).use { case (queue, _) => Eff.liftF(queue.take) }

end SignalStream
