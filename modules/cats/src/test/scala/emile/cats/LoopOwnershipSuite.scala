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

/** Tests for shared loop identity.
  *
  * All workers share one libuv loop. These tests verify that the shared loop is consistently
  * accessible and distinguishable from foreign loops.
  */
class LoopOwnershipSuite extends EmileSuite:

  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("integrated loop is the shared loop") {
    runEff {
      EmileLoop.integrated.use { loop =>
        LibuvPollingSystem.LoopAccess.get.flatMap { access =>
          access.ownsLoop(loop).flatMap { owned =>
            Eff.succeed[IO, EmileError, Unit](
              assert(owned, "Integrated loop should be the shared loop")
            )
          }
        }
      }
    }
  }

  test("foreign loop is not the shared loop") {
    runEff {
      ManagedLoop.create.use { managed =>
        managed.submit(loop => Right(loop)).flatMap { foreignLoop =>
          LibuvPollingSystem.LoopAccess.get.flatMap { access =>
            access.ownsLoop(foreignLoop).flatMap { owned =>
              Eff.succeed[IO, EmileError, Unit](
                assert(!owned, "Foreign loop should not be the shared loop")
              )
            }
          }
        }
      }
    }
  }

  test("withLoop provides the shared loop") {
    EmileIOApp.withLoop { loop =>
      LibuvPollingSystem.LoopAccess.get.flatMap { access =>
        access.ownsLoop(loop).flatMap { owned =>
          Eff.succeed[IO, EmileError, Unit](
            assert(owned, "withLoop must provide the shared loop")
          )
        }
      }
    }.either
  }

end LoopOwnershipSuite
