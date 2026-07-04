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

import boilerplate.effect.EffIO
import cats.effect.IO

/** Covers [[Emile.runtime]] and the standalone [[Emile.runEff]] runner: the resource builds a fresh
  * libuv `IORuntime` and shuts it down on release, and `runEff` returns the effect's typed result.
  */
final class EmileSpec extends EmileSuite:

  test("runtime resource acquires a live libuv IORuntime and releases it") {
    Emile.runtime.use(rt => IO.cede.evalOn(rt.compute)).void
  }

  test("runEff returns a success as Right") {
    IO.blocking(Emile.runEff(EffIO.succeed(42): EmIO[EmileError, Int]))
      .assertEquals(Right(42): Either[EmileError, Int])
  }

  test("runEff returns a typed failure as Left rather than raising it") {
    IO.blocking(Emile.runEff(EffIO.fail(EmileError.IO.EndOfStream): EmIO[EmileError, Unit]))
      .assertEquals(Left(EmileError.IO.EndOfStream): Either[EmileError, Unit])
  }
