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

import scala.concurrent.duration.*

import cats.effect.IO

// Covers the Memory queries: total and free physical RAM, memory available to the process, and the
// constrained-budget sentinel mapping.
final class MemorySpec extends EmileSuite:

  test("total and free physical memory report positive figures") {
    (
      for
        total <- Memory.total.absolve
        free <- Memory.free.absolve
        _ <- IO(assert(total > 0L))
        _ <- IO(assert(free > 0L))
        _ <- IO(assert(free <= total))
      yield ()
    ).timeout(5.seconds)
  }

  test("available memory is positive and does not exceed total") {
    (
      for
        total <- Memory.total.absolve
        available <- Memory.available.absolve
        _ <- IO(assert(available > 0L))
        _ <- IO(assert(available <= total))
      yield ()
    ).timeout(5.seconds)
  }

  test("the constrained budget is absent or a positive figure") {
    Memory.constrained.absolve
      .flatMap(budget => IO(assert(budget.forall(_ > 0L))))
      .timeout(5.seconds)
  }

end MemorySpec
