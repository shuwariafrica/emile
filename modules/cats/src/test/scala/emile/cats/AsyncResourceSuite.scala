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

/** Tests for AsyncResource - async handle lifecycle management. */
class AsyncResourceSuite extends EmileSuite:
  // scalafix:off

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("AsyncResource.make acquires and releases async handle") {
    runEff {
      EmileLoop.integrated.use { implicit loop =>
        AsyncResource.make(() => ()).use { async =>
          Eff.succeed[IO, EmileError, Unit] {
            assert(!async.isClosing, "Async should not be closing")
          }
        }
      }
    }
  }

  test("AsyncResource.make creates handle with correct type") {
    runEff {
      EmileLoop.integrated.use { implicit loop =>
        AsyncResource.make(() => ()).use { async =>
          Eff.succeed[IO, EmileError, Unit] {
            assertEquals(async.handleType, emile.HandleType.Async)
          }
        }
      }
    }
  }

  test("AsyncResource callback is invoked on send") {
    runEff {
      EmileLoop.integrated.use { implicit loop =>
        import cats.effect.Ref
        for
          counter <- Eff.liftF[IO, EmileError, Ref[IO, Int]](Ref.of[IO, Int](0))
          _ <- AsyncResource
                 .make { () =>
                   counter.update(_ + 1).unsafeRunAndForget()
                 }
                 .use { async =>
                   Eff.liftF[IO, EmileError, Unit](IO {
                     val _ = async.send
                   }) *>
                     // Give the callback a chance to run
                     Eff.liftF[IO, EmileError, Unit](IO.sleep(scala.concurrent.duration.DurationInt(50).millis)) *>
                     Eff.liftF[IO, EmileError, Int](counter.get).flatMap { count =>
                       Eff.succeed[IO, EmileError, Unit](assert(count >= 1, s"Callback should have been invoked, got $count"))
                     }
                 }
        yield ()
        end for
      }
    }
  }

  test("AsyncResource can send multiple times") {
    runEff {
      EmileLoop.integrated.use { implicit loop =>
        import cats.effect.Ref
        for
          counter <- Eff.liftF[IO, EmileError, Ref[IO, Int]](Ref.of[IO, Int](0))
          _ <- AsyncResource
                 .make { () =>
                   counter.update(_ + 1).unsafeRunAndForget()
                 }
                 .use { async =>
                   Eff.liftF[IO, EmileError, Unit](IO {
                     (1 to 5).foreach { _ =>
                       val _ = async.send
                     }
                   }) *>
                     Eff.liftF[IO, EmileError, Unit](IO.sleep(scala.concurrent.duration.DurationInt(100).millis)) *>
                     Eff.liftF[IO, EmileError, Int](counter.get).flatMap { count =>
                       // Multiple sends may coalesce, but at least one should fire
                       Eff.succeed[IO, EmileError, Unit](assert(count >= 1, s"At least one callback should have fired, got $count"))
                     }
                 }
        yield ()
        end for
      }
    }
  }

  test("Multiple AsyncResource instances can coexist") {
    runEff {
      EmileLoop.integrated.use { implicit loop =>
        AsyncResource.make(() => ()).use { async1 =>
          AsyncResource.make(() => ()).use { async2 =>
            Eff.succeed[IO, EmileError, Unit] {
              assert(!async1.isClosing && !async2.isClosing)
              assertNotEquals(async1.ptrUnsafe, async2.ptrUnsafe)
            }
          }
        }
      }
    }
  }

end AsyncResourceSuite
