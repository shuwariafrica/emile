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

  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("AsyncResource.make acquires and releases async handle") {
    runEff {
      AsyncResource.make(() => ()).use { async =>
        Eff.succeed[IO, EmileError, Unit] {
          assert(!async.isClosing, "Async should not be closing")
        }
      }
    }
  }

  test("AsyncResource.make creates handle with correct type") {
    runEff {
      AsyncResource.make(() => ()).use { async =>
        Eff.succeed[IO, EmileError, Unit] {
          assertEquals(async.handleType, emile.HandleType.Async)
        }
      }
    }
  }

  test("AsyncResource.withQueue callback is observable via queue.take") {
    runEff {
      AsyncResource.withQueue.use { case (async, queue) =>
        for
          _ <- Eff.liftF[IO, EmileError, Unit](IO(async.send: Unit))
          _ <- Eff.liftF[IO, EmileError, Unit](queue.take)
        yield ()
      }
    }
  }

  test("AsyncResource.withQueue multiple sends are observable") {
    runEff {
      AsyncResource.withQueue.use { case (async, queue) =>
        for
          _ <- Eff.liftF[IO, EmileError, Unit](IO {
                 val _ = async.send
                 val _ = async.send
                 val _ = async.send
               })
          // libuv may coalesce sends, but at least one notification arrives
          _ <- Eff.liftF[IO, EmileError, Unit](queue.take)
        yield ()
      }
    }
  }

  test("Multiple AsyncResource instances can coexist") {
    runEff {
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

end AsyncResourceSuite
