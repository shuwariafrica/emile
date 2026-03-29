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
import emile.Timeout

/** Tests for TimerResource - timer handle lifecycle management. */
class TimerResourceSuite extends EmileSuite:

  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("TimerResource.make acquires and releases timer") {
    runEff {
      TimerResource.make.use { timer =>
        Eff.succeed[IO, EmileError, Unit] {
          assert(!timer.isClosing, "Timer should not be closing")
        }
      }
    }
  }

  test("TimerResource.make creates timer with correct handle type") {
    runEff {
      TimerResource.make.use { timer =>
        Eff.succeed[IO, EmileError, Unit] {
          assertEquals(timer.handleType, emile.HandleType.Timer)
        }
      }
    }
  }

  test("TimerResource.after fires callback after delay") {
    runEff {
      import java.util.concurrent.atomic.AtomicBoolean
      val fired = new AtomicBoolean(false)
      TimerResource.after(Timeout.millis(5))(() => fired.set(true)).use { _ =>
        // IO.sleep uses cats-effect's scheduler which ticks the libuv loop;
        // sleep(10ms) guarantees the 5ms timer has fired and been processed
        Eff.liftF[IO, EmileError, Unit](IO.sleep(10.millis)) *>
          Eff.succeed[IO, EmileError, Unit](assert(fired.get(), "Timer callback should have fired"))
      }
    }
  }

  test("TimerResource.after with zero timeout fires on next poll tick") {
    runEff {
      import java.util.concurrent.atomic.AtomicBoolean
      val fired = new AtomicBoolean(false)
      TimerResource.after(Timeout.millis(0))(() => fired.set(true)).use { _ =>
        Eff.liftF[IO, EmileError, Unit](EmileLoop.tick) *>
          Eff.succeed[IO, EmileError, Unit](assert(fired.get(), "Zero-timeout timer should have fired"))
      }
    }
  }

  test("Multiple TimerResource instances can coexist") {
    runEff {
      TimerResource.make.use { timer1 =>
        TimerResource.make.use { timer2 =>
          Eff.succeed[IO, EmileError, Unit] {
            assert(!timer1.isClosing && !timer2.isClosing)
            assertNotEquals(timer1.ptrUnsafe, timer2.ptrUnsafe)
          }
        }
      }
    }
  }

end TimerResourceSuite
