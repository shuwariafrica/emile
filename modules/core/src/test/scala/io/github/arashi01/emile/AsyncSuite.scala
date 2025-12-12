/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import munit.FunSuite

/**
 * Tests for Async handle operations.
 *
 * These tests link to and execute the real libuv library.
 */
class AsyncSuite extends FunSuite:

  test("Async.init creates a valid async handle"):
    var callbackInvoked = false

    val result = for
      loop <- Loop.create
      async <- Async.init(loop) { () =>
        callbackInvoked = true
      }
      _ = async.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Async.send wakes up the event loop"):
    var wokenUp = false

    val result = for
      loop <- Loop.create
      async <- Async.init(loop) { () =>
        wokenUp = true
      }
      // Send signal before running loop
      _ <- async.send
      // Run loop to process the signal
      _ <- loop.run(RunMode.NoWait)
      _ = async.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(wokenUp, "Async callback should have been invoked after send")

  test("Multiple sends may coalesce"):
    var invokeCount = 0

    val result = for
      loop <- Loop.create
      async <- Async.init(loop) { () =>
        invokeCount += 1
      }
      // Send multiple signals
      _ <- async.send
      _ <- async.send
      _ <- async.send
      // Run loop
      _ <- loop.run(RunMode.NoWait)
      _ = async.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield invokeCount

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { count =>
      // Multiple sends may coalesce into one callback
      assert(count >= 1, s"Callback should be invoked at least once, got $count")
      // Note: We can't assert count == 1 because coalescing is not guaranteed
    }

  test("Async Handle operations work"):
    val result = for
      loop <- Loop.create
      async <- Async.init(loop)(() => ())
      // Async handles are always active
      isActive = async.isActive
      hasRef1 = async.hasRef
      _ = async.unref
      hasRef2 = async.hasRef
      _ = async.ref
      hasRef3 = async.hasRef
      asyncLoop = async.loop
      handleType = async.handleType
      _ = async.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield (isActive, hasRef1, hasRef2, hasRef3, handleType)

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { case (isActive, hasRef1, hasRef2, hasRef3, handleType) =>
      assert(isActive, "Async handle should always be active")
      assert(hasRef1, "Async should be referenced by default")
      assert(!hasRef2, "Async should not be referenced after unref")
      assert(hasRef3, "Async should be referenced after ref")
      assertEquals(handleType, HandleType.Async)
    }

  test("Async with Timer integration"):
    var asyncFired = false
    var timerFired = false
    var asyncRef: Async[Open] = null.asInstanceOf[Async[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      async <- Async.init(loop) { () =>
        asyncFired = true
      }
      timer <- Timer.init(loop)
      _ = { asyncRef = async; timerRef = timer }
      // Timer will send to async when it fires
      _ <- timer.start(Duration.millis(10), Duration.Zero) { () =>
        timerFired = true
        // Close timer after firing
        val _ = timerRef.close
        // Then close async
        val _ = asyncRef.close
      }
      // Also send async directly
      _ <- async.send
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")
    assert(asyncFired, "Async callback should have fired")
    assert(timerFired, "Timer callback should have fired")

end AsyncSuite
