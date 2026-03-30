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

import scala.scalanative.posix.signal.kill
import scala.scalanative.posix.unistd.getpid

import cats.effect.IO

import boilerplate.effect.Eff

import emile.EmileError
import emile.Signal

/** Tests for SignalStream - Unix signal handling. */
class SignalStreamSuite extends EmileSuite:

  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  /** Send a signal to the current process, routing the POSIX result through the Eff channel. */
  private def sendSignal(signum: Int): Eff[IO, EmileError, Unit] =
    Eff.suspend[IO, EmileError, Int](kill(getpid(), signum)).flatMap { rc =>
      if rc == 0 then Eff.unit[IO, EmileError]
      else Eff.fail[IO, EmileError, Unit](EmileError.SystemError(emile.ErrorCode(rc), s"kill() returned $rc"))
    }

  test("SignalStream.watch acquires signal handler") {
    runEff {
      SignalStream.watch(Signal.SIGUSR1).use { case (_, ready) =>
        Eff.liftF[IO, EmileError, Unit](ready)
      }
    }
  }

  test("SignalStream.watch receives signal via queue.take") {
    runEff {
      SignalStream.watch(Signal.SIGUSR2).use { case (queue, ready) =>
        for
          _ <- Eff.liftF[IO, EmileError, Unit](ready)
          _ <- sendSignal(Signal.SIGUSR2)
          _ <- Eff.liftF[IO, EmileError, Unit](queue.take)
        yield ()
      }
    }
  }

  test("SignalStream.awaitOnce receives single signal") {
    runEff {
      SignalStream.watch(Signal.SIGUSR2).use { case (queue, ready) =>
        for
          _ <- Eff.liftF[IO, EmileError, Unit](ready)
          _ <- sendSignal(Signal.SIGUSR2)
          _ <- Eff.liftF[IO, EmileError, Unit](queue.take)
        yield ()
      }
    }
  }

  test("SignalStream.watch receives multiple signals") {
    runEff {
      SignalStream.watch(Signal.SIGUSR2).use { case (queue, ready) =>
        for
          _ <- Eff.liftF[IO, EmileError, Unit](ready)
          _ <- sendSignal(Signal.SIGUSR2)
          _ <- Eff.liftF[IO, EmileError, Unit](queue.take)
          _ <- sendSignal(Signal.SIGUSR2)
          _ <- Eff.liftF[IO, EmileError, Unit](queue.take)
        yield ()
      }
    }
  }

  test("SignalStream.watch cleans up handler on release") {
    runEff {
      val resource = SignalStream.watch(Signal.SIGUSR1)
      resource.use { case (_, ready) =>
        Eff.liftF[IO, EmileError, Unit](ready)
      } *>
        resource.use { case (_, ready) =>
          Eff.liftF[IO, EmileError, Unit](ready)
        }
    }
  }

  test("Multiple SignalStream watchers for different signals") {
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

  test("Process survives signal sent during resource use followed by normal release") {
    runEff {
      SignalStream.watch(Signal.SIGUSR2).use { case (_, ready) =>
        for
          _ <- Eff.liftF[IO, EmileError, Unit](ready)
          _ <- sendSignal(Signal.SIGUSR2)
          _ <- Eff.liftF[IO, EmileError, Unit](IO.cede)
        yield ()
      } *>
        Eff.succeed[IO, EmileError, Unit](())
    }
  }

end SignalStreamSuite
