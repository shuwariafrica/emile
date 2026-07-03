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

import cats.effect.IO
import cats.syntax.all.*

import emile.EmileSuite
import emile.LibUVPollingSystem

/** Covers [[Routing.onOwner]]: a thunk routed to a live poller runs and returns its value, many
  * concurrent routings all complete, a cancelled routing completes without deadlock, and a worker
  * retired by a blocking region loses its owner claim so routing falls back to the submit path.
  */
final class RoutingSpec extends EmileSuite:

  test("onOwner runs the thunk on the owning loop and returns its value") {
    LibUVPollingSystem.currentPoller.flatMap(poller => Routing.onOwner(poller)(21 * 2)).assertEquals(42)
  }

  test("concurrent onOwner routings all complete") {
    LibUVPollingSystem.currentPoller
      .flatMap(poller => List.range(0, 64).parTraverse(n => Routing.onOwner(poller)(n)))
      .map(_.sum)
      .assertEquals((0 until 64).sum)
  }

  test("a cancelled onOwner routing completes without deadlock") {
    LibUVPollingSystem.currentPoller.flatMap(poller => Routing.onOwner(poller)(()).start.flatMap(_.cancel))
  }

  test("a worker retired by a blocking region loses its owner claim, and routing still completes") {
    // IO.blocking retires the worker (cats-effect hands its poller to a replacement thread) yet the
    // fibre continues on the retiree; a stale owner claim there would race the replacement's uv_run.
    def landOnOwner(attempt: Int): IO[LibUVPoller] =
      LibUVPollingSystem.currentPoller.flatMap(poller =>
        if poller.isOwnerThread then IO.pure(poller)
        else if attempt >= 5000 then IO.raiseError(new IllegalStateException("no poller-owner thread reached"))
        else IO.cede >> landOnOwner(attempt + 1)
      )
    for
      poller <- landOnOwner(0)
      _ <- IO.blocking(())
      _ <- IO(assert(!poller.isOwnerThread, "a retired blocker thread must not claim loop ownership"))
      routed <- Routing.onOwner(poller)(21 * 2)
      _ <- IO(assertEquals(routed, 42))
    yield ()
  }

end RoutingSpec
