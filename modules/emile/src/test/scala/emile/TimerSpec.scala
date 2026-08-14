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

// Covers [[Timer]]: a delay completes, and an interval emits once per period.
final class TimerSpec extends EmileSuite:

  test("after completes") {
    Timer.after(20.millis).absolve
  }

  test("interval emits once per period") {
    Timer.interval(15.millis).take(3L).compile.count.absolve.assertEquals(3L)
  }
