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

import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.syntax.all.*

import emile.*

// Offload-lane responsiveness: a 5 ms timer tick under saturating CPU bursts, inline on the loop
// workers versus offloaded, asserting the offloaded tick stays within an order of magnitude of inline.
final class OffloadResponsivenessSpec extends StressSuite:

  private val cores = java.lang.Runtime.getRuntime.availableProcessors()

  // A busy-spin the optimiser cannot elide - the un-observable result keeps the loop from folding away.
  private def spin(ms: Long): Unit =
    val end = System.nanoTime() + ms * 1_000_000L
    var x = 0L
    while System.nanoTime() < end do x += 1
    if x == -1L then println("")

  // The maximum delay beyond a 5 ms tick, over `n` ticks, in milliseconds.
  private def tickDrift(n: Int): IO[Long] =
    val once: IO[Long] =
      for
        t0 <- IO.monotonic
        _ <- IO.sleep(5.millis)
        t1 <- IO.monotonic
      yield (t1 - t0).toMillis - 5
    (1 to n).toList.traverse(_ => once).map(_.max)

  private def heavyBurst(run: IO[Unit] => IO[Unit]): IO[Unit] =
    (1 to cores * 4).toList.parTraverse_(_ => run(IO.delay(spin(100))))

  test("offloaded CPU bursts keep the timer tick responsive - the same order of magnitude as inline") {
    for
      inlineDrift <- (tickDrift(30), heavyBurst(identity)).parMapN((drift, _) => drift)
      offloadDrift <- (tickDrift(30), heavyBurst(io => EffIO.liftF(io).offload.absolve)).parMapN((drift, _) => drift)
    yield
      // Offloading must keep the tick within an order of magnitude of the inline baseline; the generous
      // bound tolerates CI variance while still catching a catastrophic scheduling regression.
      assert(offloadDrift <= inlineDrift * 10 + 100, s"inline=${inlineDrift}ms offloaded=${offloadDrift}ms")
  }

end OffloadResponsivenessSpec
