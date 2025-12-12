/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import munit.FunSuite
import scala.collection.mutable.ArrayBuffer

/**
 * Tests for Timer handle operations.
 *
 * These tests link to and execute the real libuv library.
 */
class TimerSuite extends FunSuite:

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
      _ = { timerRef = timer }
      _ <- timer.start(Duration.millis(10), Duration.Zero) { () =>
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
      timer <- Timer.after(loop, Duration.millis(5)) { () =>
        fired = true
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
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
      _ = { timerRef = timer }
      _ <- timer.start(Duration.millis(5), Duration.millis(5)) { () =>
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
      _ <- timer.start(Duration.millis(100), Duration.Zero) { () =>
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
      _ = timer.setRepeat(Duration.millis(100))
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
      _ <- timer.start(Duration.millis(1000), Duration.Zero)(() => ())
      dueIn = timer.dueIn
      _ <- timer.stop
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield dueIn

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { dueIn =>
      // Should be close to 1000ms (allowing some tolerance)
      assert(dueIn.toMillis > 900 && dueIn.toMillis <= 1000,
        s"dueIn should be close to 1000ms, got ${dueIn.toMillis}")
    }

  test("Timer Handle operations work"):
    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      // Test Handle operations
      isActive1 = timer.isActive
      _ <- timer.start(Duration.millis(100), Duration.Zero)(() => ())
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
      _ = { timer1Ref = timer1; timer2Ref = timer2; timer3Ref = timer3 }
      _ <- timer3.start(Duration.millis(30), Duration.Zero) { () =>
        order += 3
        val _ = timer3Ref.close
      }
      _ <- timer1.start(Duration.millis(10), Duration.Zero) { () =>
        order += 1
        val _ = timer1Ref.close
      }
      _ <- timer2.start(Duration.millis(20), Duration.Zero) { () =>
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

  test("restarting timer does not leak callbacks"):
    import io.github.arashi01.emile.unsafe.CallbackRegistry

    val result = for
      loop <- Loop.create
      timer <- Timer.init(loop)
      // Record initial registry size
      initialSize = CallbackRegistry.size
      // Start timer multiple times - should not grow registry
      _ <- timer.start(Duration.millis(100), Duration.Zero)(() => ())
      sizeAfterFirst = CallbackRegistry.size
      _ <- timer.start(Duration.millis(100), Duration.Zero)(() => ())
      sizeAfterSecond = CallbackRegistry.size
      _ <- timer.start(Duration.millis(100), Duration.Zero)(() => ())
      sizeAfterThird = CallbackRegistry.size
      _ <- timer.stop
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield (initialSize, sizeAfterFirst, sizeAfterSecond, sizeAfterThird)

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { case (initial, first, second, third) =>
      // Each restart should replace, not add to registry
      assertEquals(first, initial + 1, "First start should add one callback")
      assertEquals(second, initial + 1, "Second start should replace, not add")
      assertEquals(third, initial + 1, "Third start should replace, not add")
    }

end TimerSuite
