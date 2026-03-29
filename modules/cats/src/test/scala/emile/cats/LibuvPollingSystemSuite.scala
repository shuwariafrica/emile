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
import cats.effect.kernel.Resource

import munit.CatsEffectSuite

class LibuvPollingSystemSuite extends CatsEffectSuite:

  test("polling system configs are instance-local") {
    val cfgA = LoopConfig.empty.copy(metricsEnabled = Some(true))
    val cfgB = LoopConfig.empty

    val sysA = LibuvPollingSystem(cfgA)
    val sysB = LibuvPollingSystem(cfgB)

    assertEquals(sysA.loopConfig.metricsEnabled, Some(true))
    assertEquals(sysB.loopConfig.metricsEnabled, None)
  }

  test("poller lifecycle uses captured config per runtime") {
    val cfg = LoopConfig.empty.copy(metricsEnabled = Some(true))
    val sys = LibuvPollingSystem(cfg)

    val acquire = IO.blocking(sys.makePoller())
    val release = (p: LibuvPollingSystem.LibuvPoller) => IO.blocking(sys.closePoller(p))

    Resource.make(acquire)(release).use { poller =>
      IO {
        // Freshly created pollers start with no active handles; loop is idle but valid.
        assert(!poller.loop.isAlive, "new poller should start idle with no handles")
        assertEquals(sys.loopConfig.metricsEnabled, Some(true))
      }
    }
  }
end LibuvPollingSystemSuite
