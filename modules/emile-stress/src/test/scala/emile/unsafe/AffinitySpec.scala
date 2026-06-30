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

import emile.LibUVPollingSystem

/** Guards the load-bearing invariant: a thunk routed through `Routing.onOwner` always runs on the
  * thread that owns its loop, even under heavy concurrent scheduling. Each operation reports whether
  * it observed `isOwnerThread`; the pre-fix fast path occasionally migrated a thunk to a stealing
  * worker, the fixed single-node path never does.
  */
final class AffinitySpec extends StressSuite:

  // Large enough that the minimum auto-cede thresholds fire repeatedly and worker local queues stay
  // busy - the regime in which the pre-fix node could be stolen.
  private val operations = 20000

  test("every Routing.onOwner thunk runs on its owner loop thread under concurrent load") {
    LibUVPollingSystem.currentPoller.flatMap: poller =>
      List
        .range(0, operations)
        .parTraverse(_ => Routing.onOwner(poller)(poller.isOwnerThread))
        .map: results =>
          val offThread = results.count(onOwner => !onOwner)
          assert(offThread == 0, s"$offThread / $operations Routing.onOwner thunks executed off the owner loop thread")
  }

end AffinitySpec
