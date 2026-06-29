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

import cats.effect.unsafe.IORuntime

import munit.CatsEffectSuite

/** Base suite for emile-fs2's cats-effect tests: every test runs on one shared libuv-backed
  * `IORuntime`. The emile-fs2 module does not depend on the `emile` test classpath, so the
  * `EmileSuite` pattern is duplicated here over the same `Emile.unsafeRuntime` factory.
  */
abstract class Fs2EmileSuite extends CatsEffectSuite:
  implicit override lazy val munitIORuntime: IORuntime = Fs2EmileSuite.SharedRuntime

/** Holds the process-shared libuv [[cats.effect.unsafe.IORuntime IORuntime]] for [[Fs2EmileSuite]]. */
@scala.annotation.internal.sharable
object Fs2EmileSuite:

  /** One libuv `IORuntime` shared across every [[Fs2EmileSuite]]; a shutdown hook releases it when
    * the test binary exits.
    */
  lazy val SharedRuntime: IORuntime =
    val rt = Emile.unsafeRuntime(LoopConfig.default)
    java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => rt.shutdown(), "emile-fs2-test-shutdown"))
    rt
