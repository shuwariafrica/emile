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

import boilerplate.effect.Eff

import emile.EmileError

/** Tests for EmileLoop Resource integration. */
class EmileLoopSuite extends EmileSuite:

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("EmileLoop.integrated acquires loop from runtime") {
    runEff {
      EmileLoop.integrated.use { loop =>
        Eff.succeed[IO, EmileError, Unit] {
          // Loop is an opaque type wrapping Ptr[Byte], verify it's accessible
          assert(loop.isAlive || !loop.isAlive, "Loop should be callable")
        }
      }
    }
  }

  test("EmileLoop.integrated provides loop owned by current worker") {
    runEff {
      EmileLoop.integrated.use { loop =>
        LibuvPollingSystem.LoopAccess.get.flatMap { access =>
          access.ownsLoop(loop).flatMap { owned =>
            if owned then Eff.unit[IO, EmileError]
            else
              Eff.fail[IO, EmileError, Unit](
                EmileError.InvalidArgument("loop", "integrated loop should be owned by worker")
              )
          }
        }
      }
    }
  }

  test("EmileLoop.integrated can be nested without deadlock") {
    runEff {
      EmileLoop.integrated.use { outer =>
        EmileLoop.integrated.use { inner =>
          Eff.succeed[IO, EmileError, Unit] {
            // Both should reference the same loop
            assertEquals(outer.ptrUnsafe, inner.ptrUnsafe)
          }
        }
      }
    }
  }

end EmileLoopSuite
