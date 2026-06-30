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

import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.IORuntimeBuilder
import cats.effect.unsafe.IORuntimeConfig

import munit.CatsEffectSuite

import emile.LibUVPollingSystem
import emile.LoopConfig

/** Base for emile's concurrency-invariant suites: every test runs on a libuv `IORuntime` forced to
  * the minimum auto-cede thresholds, so scheduler races surface. The aggression is load-bearing - at
  * the default thresholds the `Routing.onOwner` affinity defect cannot reproduce and AffinitySpec
  * passes vacuously - so do not relax it.
  */
abstract class StressSuite extends CatsEffectSuite:
  implicit override lazy val munitIORuntime: IORuntime = StressSuite.AggressiveRuntime

/** Holds the shared aggressive-auto-cede libuv [[cats.effect.unsafe.IORuntime IORuntime]] for [[StressSuite]]. */
@scala.annotation.internal.sharable
object StressSuite:

  /** Forced to the minimum `cancelationCheckThreshold` / `autoYieldThreshold` (`2`) so auto-cede fires
    * on nearly every runloop step. Shared across every [[StressSuite]]; a shutdown hook releases it.
    */
  lazy val AggressiveRuntime: IORuntime =
    val rt = IORuntimeBuilder()
      .setPollingSystem(LibUVPollingSystem(LoopConfig.default))
      .setConfig(IORuntimeConfig().copy(cancelationCheckThreshold = 2, autoYieldThreshold = 2))
      .build()
    java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => rt.shutdown(), "emile-stress-shutdown"))
    rt
