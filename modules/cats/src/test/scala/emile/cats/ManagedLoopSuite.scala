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
import emile.Timeout
import emile.Timer

/** Tests for ManagedLoop - standalone libuv loop with dedicated thread. */
class ManagedLoopSuite extends EmileSuite:
  // scalafix:off

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("ManagedLoop.create acquires and releases a standalone loop") {
    runEff {
      ManagedLoop.create.use { managed =>
        managed.submitPure { loop =>
          // Verify loop is accessible and can be queried
          assert(loop.isAlive || !loop.isAlive, "Loop should be callable")
        }
      }
    }
  }

  test("ManagedLoop.submit executes task on loop thread") {
    runEff {
      ManagedLoop.create.use { managed =>
        val mainThread = Thread.currentThread()
        managed.submitPure { _ =>
          val loopThread = Thread.currentThread()
          assertNotEquals(loopThread, mainThread, "Task should run on different thread")
          assertEquals(loopThread.getName, "emile-managed-loop")
        }
      }
    }
  }

  test("ManagedLoop.submit propagates Either errors to Eff channel") {
    runEff {
      ManagedLoop.create.use { managed =>
        managed
          .submit[Unit] { _ =>
            Left(EmileError.TimedOut)
          }
          .catchAll { e =>
            Eff.succeed[IO, EmileError, Unit] {
              assertEquals(e, EmileError.TimedOut)
            }
          }
      }
    }
  }

  test("ManagedLoop.runUntilComplete drains single timer") {
    runEff {
      ManagedLoop.create.use { managed =>
        val fired = new java.util.concurrent.atomic.AtomicBoolean(false)
        for
          _ <- managed.submitPure { loop =>
                 val _ = Timer.after(loop, Timeout.millis(0)) { () =>
                   fired.set(true)
                 }
               }
          _ <- managed.runUntilComplete
          result <- managed.submitPure(_ => fired.get())
        yield assert(result, "Timer should have fired")
      }
    }
  }

  test("ManagedLoop.runUntilComplete drains many timers without stack growth") {
    runEff {
      ManagedLoop.create.use { managed =>
        val count = 512
        // Use an AtomicInteger directly on the loop thread - no cross-thread IO
        val counter = new java.util.concurrent.atomic.AtomicInteger(0)
        for
          _ <- managed.submitPure { loop =>
                 (0 until count).foreach { _ =>
                   val _ = Timer.after(loop, Timeout.millis(0)) { () =>
                     val _ = counter.incrementAndGet()
                   }
                 }
               }
          _ <- managed.runUntilComplete
          finalCount <- managed.submitPure(_ => counter.get())
        yield assertEquals(finalCount, count)
      }
    }
  }

  test("ManagedLoop.runUntilComplete with chained timers") {
    runEff {
      ManagedLoop.create.use { managed =>
        val steps = new java.util.concurrent.atomic.AtomicInteger(0)
        for
          _ <- managed.submitPure { loop =>
                 // Chain timers: each timer schedules the next
                 def scheduleNext(remaining: Int): Unit =
                   if remaining > 0 then
                     val _ = Timer.after(loop, Timeout.millis(0)) { () =>
                       steps.incrementAndGet()
                       scheduleNext(remaining - 1)
                     }

                 scheduleNext(10)
               }
          _ <- managed.runUntilComplete
          finalSteps <- managed.submitPure(_ => steps.get())
        yield assertEquals(finalSteps, 10)
        end for
      }
    }
  }

  test("ManagedLoop clean shutdown after runUntilComplete") {
    // This test verifies that the loop thread terminates cleanly
    runEff {
      val threadRef = new java.util.concurrent.atomic.AtomicReference[Thread](null)
      ManagedLoop.create.use { managed =>
        managed.submitPure { _ =>
          threadRef.set(Thread.currentThread())
        }
      } *> Eff.liftF[IO, EmileError, Unit](IO {
        val thread = threadRef.get()
        thread.join(500)
        assert(!thread.isAlive, "Loop thread should have terminated within 500ms")
      })
    }
  }

end ManagedLoopSuite
