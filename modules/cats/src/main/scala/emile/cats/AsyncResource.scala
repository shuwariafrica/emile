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

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Queue
import cats.effect.std.unsafe.UnboundedQueue

import boilerplate.effect.*
import boilerplate.effect.Eff

import emile.Async
import emile.EmileError
import emile.Open

/** cats-effect Resource integration for Async handles.
  *
  * All libuv operations are serialised through `EffAsync.onLoop` / `EffAsync.closeHandle` to
  * prevent races with the shared event loop.
  */
object AsyncResource:

  /** Create an async handle as a managed resource with a raw callback.
    *
    * @param callback The callback to invoke when the async is signalled
    */
  def make(callback: () => Unit): Resource[Eff.Of[IO, EmileError], Async[Open]] =
    Resource.make(
      acquire = EffAsync.onLoop(loop => Async.init(loop)(callback))
    )(
      release = h => EffAsync.closeHandle(h.closeAsync)
    )

  /** Create an async handle with an IO-bridged notification queue.
    *
    * Each `send` on the returned handle offers a unit to the queue. Consumers observe sends via
    * `queue.take` with no sleeps or polling.
    *
    * @return Resource providing (handle, queue) where queue receives notifications on each send
    */
  def withQueue: Resource[Eff.Of[IO, EmileError], (Async[Open], Queue[IO, Unit])] =
    Resource
      .eval(Eff.liftF[IO, EmileError, UnboundedQueue[IO, Unit]](Queue.unsafeUnbounded[IO, Unit]))
      .flatMap { queue =>
        make(() => queue.unsafeOffer(())).map(handle => (handle, queue: Queue[IO, Unit]))
      }

end AsyncResource
