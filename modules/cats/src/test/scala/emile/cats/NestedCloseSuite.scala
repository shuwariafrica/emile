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

import cats.effect.Deferred
import cats.effect.IO

import boilerplate.effect.Eff

import emile.EmileError

/** Regression tests for resource cleanup under various exit conditions. */
class NestedCloseSuite extends EmileSuite:

  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("TimerResource releases cleanly under cancellation") {
    runEff {
      for
        acquired <- Eff.liftF[IO, EmileError, Deferred[IO, Unit]](Deferred[IO, Unit])
        fiber <- TimerResource.make.use { _ =>
                   // Signal that the resource is acquired, then block
                   Eff.liftF[IO, EmileError, Boolean](acquired.complete(())) *>
                     Eff.liftF[IO, EmileError, Unit](IO.never)
                 }.start
        // Wait for the resource to be acquired (no sleep hack)
        _ <- Eff.liftF[IO, EmileError, Unit](acquired.get)
        _ <- fiber.cancel
        // If the resource leaked, this would fail or hang
        _ <- TimerResource.make.use(_ => Eff.unit[IO, EmileError])
      yield ()
    }
  }

  test("TcpResource releases cleanly under error") {
    runEff {
      val program = TcpResource.make.use { _ =>
        Eff.fail[IO, EmileError, Unit](EmileError.TimedOut)
      }
      program.catchAll(_ => Eff.unit[IO, EmileError]) *>
        TcpResource.make.use(_ => Eff.unit[IO, EmileError])
    }
  }

end NestedCloseSuite
