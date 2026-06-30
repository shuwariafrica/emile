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

import cats.syntax.all.*

import emile.EmileSuite
import emile.LibUVPollingSystem

/** Covers [[Routing.onOwner]]: a thunk routed to a live poller runs and returns its value, many
  * concurrent routings all complete, and a cancelled routing completes without deadlock.
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

end RoutingSpec
