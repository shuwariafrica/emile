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

import scala.concurrent.duration.*

import cats.effect.IO

import boilerplate.effect.Eff

import emile.EmileError
import emile.Signal

/** Regression tests for resource cleanup under various exit conditions. */
class NestedCloseSuite extends EmileSuite:

  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("nested SignalStream.watch releases cleanly") {
    runEff {
      SignalStream.watch(Signal.SIGUSR1).use { case (_, ready1) =>
        SignalStream.watch(Signal.SIGUSR2).use { case (_, ready2) =>
          for
            _ <- Eff.liftF[IO, EmileError, Unit](ready1)
            _ <- Eff.liftF[IO, EmileError, Unit](ready2)
          yield ()
        }
      }
    }
  }

  test("TimerResource releases cleanly under cancellation") {
    // Start a fiber holding a timer resource, cancel it, verify the
    // loop remains stable (no leaked handles, no crash)
    runEff {
      for
        fiber <- TimerResource.make.use { _ =>
                   // Block indefinitely inside the resource
                   Eff.liftF[IO, EmileError, Unit](IO.never)
                 }.start
        // Give the fiber time to acquire the resource
        _ <- Eff.liftF[IO, EmileError, Unit](IO.sleep(20.millis))
        _ <- fiber.cancel
        // If the resource leaked or the close path is broken,
        // subsequent resource acquisition would fail or hang
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
        // Verify the loop is stable after error-path release
        TcpResource.make.use(_ => Eff.unit[IO, EmileError])
    }
  }

end NestedCloseSuite
