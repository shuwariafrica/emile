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

import cats.effect.IO

/** Covers [[Emile.runtime]]: the resource builds a fresh libuv `IORuntime` and shuts it down on
  * release, running a step on its own compute pool to confirm the runtime is live.
  */
final class EmileSpec extends EmileSuite:

  test("runtime resource acquires a live libuv IORuntime and releases it") {
    Emile.runtime.use(rt => IO.cede.evalOn(rt.compute)).void
  }
