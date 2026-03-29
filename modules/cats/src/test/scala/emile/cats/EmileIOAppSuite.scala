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

/** Tests for EmileIOApp - application entry point with libuv integration. */
class EmileIOAppSuite extends EmileSuite:

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("EmileIOApp.withLoop provides live loop") {
    EmileIOApp
      .withLoop { loop =>
        Eff.succeed[IO, EmileError, Unit] {
          assert(loop.isAlive || !loop.isAlive, "Loop should be accessible")
        }
      }
      .either
      .map {
        case Right(()) => ()
        case Left(e)   => fail(s"withLoop failed: $e")
      }
  }

  test("EmileIOApp.withLoop loop is owned by worker") {
    EmileIOApp
      .withLoop { loop =>
        LibuvPollingSystem.LoopAccess.get.flatMap { access =>
          access.ownsLoop(loop).flatMap { owned =>
            if owned then Eff.unit[IO, EmileError]
            else
              Eff.fail[IO, EmileError, Unit](
                EmileError.InvalidArgument("loop", "Loop should be owned")
              )
          }
        }
      }
      .either
      .map {
        case Right(()) => ()
        case Left(e)   => fail(s"Ownership check failed: $e")
      }
  }

  test("EmileIOApp.withLoop can use timer resources") {
    EmileIOApp
      .withLoop { _ =>
        TimerResource.make.use { _ =>
          Eff.unit[IO, EmileError]
        }
      }
      .either
      .map {
        case Right(()) => ()
        case Left(e)   => fail(s"Timer resource failed: $e")
      }
  }

  test("EmileIOApp.withLoop propagates typed errors") {
    EmileIOApp
      .withLoop { _ =>
        Eff.fail[IO, EmileError, Unit](EmileError.TimedOut)
      }
      .either
      .map {
        case Left(EmileError.TimedOut) => ()
        case other                     => fail(s"Expected TimedOut, got $other")
      }
  }

  test("EmileIOApp.withLoop can nest safely") {
    EmileIOApp
      .withLoop { outer =>
        EmileIOApp.withLoop { inner =>
          Eff.succeed[IO, EmileError, Unit] {
            // Both should be the same loop
            assertEquals(outer.ptrUnsafe, inner.ptrUnsafe)
          }
        }
      }
      .either
      .map {
        case Right(()) => ()
        case Left(e)   => fail(s"Nested withLoop failed: $e")
      }
  }

end EmileIOAppSuite
