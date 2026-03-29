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
package emile

import scala.collection.mutable.ArrayBuffer

import munit.FunSuite

/** Tests for Timer handle operations.
  *
  * These tests link to and execute the real libuv library.
  */
class TimerSuite extends FunSuite:
// scalafix:off

  test("Timer.init creates a valid timer"):
    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield timer

    assert(result.isRight, s"Expected Right, got $result")

  test("Timer fires after timeout"):
    var fired = false
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ = timerRef = timer
      _ <- timer.start(Timeout.millis(10), Timeout.Zero) { () =>
             fired = true
             // Close timer after it fires (one-shot)
             val _ = timerRef.close
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(fired, "Timer callback should have fired")

  test("Timer.after convenience method works"):
    var fired = false
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      timer <- Timer.after(loop, Timeout.millis(5)) { () =>
                 fired = true
                 val _ = timerRef.close
               }
      _ = timerRef = timer
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(fired, "Timer.after callback should have fired")

  test("Repeating timer fires multiple times"):
    val fireCount = ArrayBuffer[Int]()
    var count = 0
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ = timerRef = timer
      _ <- timer.start(Timeout.millis(5), Timeout.millis(5)) { () =>
             count += 1
             fireCount += count
             if count >= 3 then
               // Stop and close after 3 fires
               val _ = timerRef.stop
               val _ = timerRef.close
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield fireCount.toList

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { counts =>
      assertEquals(counts, List(1, 2, 3))
    }

  test("Timer.stop prevents callback"):
    var fired = false

    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ <- timer.start(Timeout.millis(100), Timeout.Zero) { () =>
             fired = true
           }
      _ <- timer.stop
      _ <- loop.run(RunMode.NoWait) // Run once without waiting
      _ = timer.close
      _ <- loop.run(RunMode.Default) // Process close
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(!fired, "Stopped timer should not fire")

  test("Timer.setRepeat changes repeat interval"):
    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ = timer.setRepeat(Timeout.millis(100))
      interval = timer.repeatInterval
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield interval

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { interval =>
      assertEquals(interval.toMillis, 100L)
    }

  test("Timer.dueIn returns time until fire"):
    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ <- timer.start(Timeout.millis(1000), Timeout.Zero)(() => ())
      dueIn = timer.dueIn
      _ <- timer.stop
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield dueIn

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { dueIn =>
      // Should be close to 1000ms (allowing some tolerance)
      assert(dueIn.toMillis > 900 && dueIn.toMillis <= 1000, s"dueIn should be close to 1000ms, got ${dueIn.toMillis}")
    }

  test("Timer Handle operations work"):
    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      // Test Handle operations
      isActive1 = timer.isActive
      _ <- timer.start(Timeout.millis(100), Timeout.Zero)(() => ())
      isActive2 = timer.isActive
      hasRef1 = timer.hasRef
      _ = timer.unref
      hasRef2 = timer.hasRef
      _ = timer.ref
      hasRef3 = timer.hasRef
      timerLoop = timer.loop
      handleType = timer.handleType
      _ <- timer.stop
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield (isActive1, isActive2, hasRef1, hasRef2, hasRef3, handleType)

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { case (isActive1, isActive2, hasRef1, hasRef2, hasRef3, handleType) =>
      assert(!isActive1, "Timer should not be active before start")
      assert(isActive2, "Timer should be active after start")
      assert(hasRef1, "Timer should be referenced by default")
      assert(!hasRef2, "Timer should not be referenced after unref")
      assert(hasRef3, "Timer should be referenced after ref")
      assertEquals(handleType, HandleType.Timer)
    }

  test("Multiple timers fire in order"):
    val order = ArrayBuffer[Int]()
    var timer1Ref: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var timer2Ref: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var timer3Ref: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      timer1 <- Timer.init(loop)
      timer2 <- Timer.init(loop)
      timer3 <- Timer.init(loop)
      _ =
        timer1Ref = timer1; timer2Ref = timer2; timer3Ref = timer3
      _ <- timer3.start(Timeout.millis(30), Timeout.Zero) { () =>
             order += 3
             val _ = timer3Ref.close
           }
      _ <- timer1.start(Timeout.millis(10), Timeout.Zero) { () =>
             order += 1
             val _ = timer1Ref.close
           }
      _ <- timer2.start(Timeout.millis(20), Timeout.Zero) { () =>
             order += 2
             val _ = timer2Ref.close
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield order.toList

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { order =>
      assertEquals(order, List(1, 2, 3), "Timers should fire in timeout order")
    }

  test("restarting timer only fires the last registered callback"):
    var fired1 = false
    var fired2 = false
    var fired3 = false
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ = timerRef = timer
      // Start timer multiple times - only the last callback should fire
      _ <- timer.start(Timeout.millis(10), Timeout.Zero) { () =>
             fired1 = true
             val _ = timerRef.close
           }
      _ <- timer.start(Timeout.millis(10), Timeout.Zero) { () =>
             fired2 = true
             val _ = timerRef.close
           }
      _ <- timer.start(Timeout.millis(10), Timeout.Zero) { () =>
             fired3 = true
             val _ = timerRef.close
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(!fired1, "First callback should not fire after being replaced")
    assert(!fired2, "Second callback should not fire after being replaced")
    assert(fired3, "Only the last registered callback should fire")

  // ===========================================================================
  // Lifecycle Safety Tests
  //
  // These tests verify timer handle lifecycle, particularly the interaction
  // between stop/close operations. While timers don't have the global tree
  // issue of signals, they still require proper callback cleanup.
  // ===========================================================================

  test("Timer.closeAsync fires callback after handle is fully closed"):
    var closeCallbackFired = false
    var timerCallbackFired = false

    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      _ <- timer.start(Timeout.millis(100), Timeout.Zero)(() => timerCallbackFired = true)
      // Stop before close (defensive pattern)
      _ <- timer.stop
      _ = timer.closeAsync(_ => closeCallbackFired = true)
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(closeCallbackFired, "Close callback must fire")
    assert(!timerCallbackFired, "Timer callback should not fire after stop")

  test("Rapid timer create/stop/close does not corrupt state"):
    // Stress test for timer lifecycle - rapidly create, stop, and close timers
    val iterations = 30
    var closeCount = 0

    val result = for
      loop <- Loop.create
      _ <- (0 until iterations).foldLeft(Right(()): Either[EmileError, Unit]) { (acc, _) =>
             acc.flatMap { _ =>
               for
                 timer <- Timer.init(loop)
                 _ <- timer.startOnce(Timeout.millis(100))(() => ())
                 _ <- timer.stop
                 _ = timer.closeAsync(_ => closeCount += 1)
                 _ <- loop.run(RunMode.NoWait)
               yield ()
             }
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield closeCount

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assertEquals(count, iterations, s"All $iterations close callbacks should fire"))

  test("Timer sequential reuse works correctly"):
    // Each timer fires then we close it before creating the next
    var totalFires = 0
    var timer1Ref: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var timer2Ref: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var timer3Ref: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      // Create timer, let it fire, close it, create new one - repeat
      timer1 <- Timer.after(loop, Timeout.millis(5)) { () =>
                  totalFires += 1
                  val _ = timer1Ref.close
                }
      _ = timer1Ref = timer1
      _ <- loop.run(RunMode.Default)
      timer2 <- Timer.after(loop, Timeout.millis(5)) { () =>
                  totalFires += 1
                  val _ = timer2Ref.close
                }
      _ = timer2Ref = timer2
      _ <- loop.run(RunMode.Default)
      timer3 <- Timer.after(loop, Timeout.millis(5)) { () =>
                  totalFires += 1
                  val _ = timer3Ref.close
                }
      _ = timer3Ref = timer3
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield totalFires

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assertEquals(count, 3, "All timers should fire"))

end TimerSuite
