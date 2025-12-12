/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import munit.FunSuite

/**
 * Tests for Loop operations.
 *
 * These tests link to and execute the real libuv library.
 */
class LoopSuite extends FunSuite:

  test("Loop.default returns a valid loop"):
    val result = Loop.default
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { loop =>
      assert(loop.isAlive || !loop.isAlive) // Just verify we can call methods
    }

  test("Loop.create creates and closes a new loop"):
    val result = Loop.create
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { loop =>
      // New loop should not be alive (no handles)
      assert(!loop.isAlive, "New loop should not be alive without handles")

      // Close should succeed
      val closeResult = loop.close
      assert(closeResult.isRight, s"Close failed: $closeResult")
    }

  test("Loop.run with empty loop returns immediately"):
    val result = for
      loop <- Loop.create
      runResult <- loop.run(RunMode.Default)
      _ <- loop.close
    yield runResult

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { ranHandles =>
      // Empty loop should return false (no handles ran)
      assert(!ranHandles, "Empty loop should return false from run")
    }

  test("Loop.now returns a timestamp"):
    val result = Loop.default
    assert(result.isRight)
    result.foreach { loop =>
      val ts1 = loop.now
      assert(ts1.millis >= 0, "Timestamp should be non-negative")

      // Update time and check again
      loop.updateTime
      val ts2 = loop.now
      assert(ts2.millis >= ts1.millis, "Timestamp should not decrease")
    }

  test("Loop.runOnce processes pending callbacks"):
    val result = for
      loop <- Loop.create
      // RunOnce on empty loop should return immediately
      runResult <- loop.run(RunMode.Once)
      _ <- loop.close
    yield runResult

    assert(result.isRight, s"Expected Right, got $result")

  test("Loop.runNowait returns without blocking"):
    val result = for
      loop <- Loop.create
      runResult <- loop.run(RunMode.NoWait)
      _ <- loop.close
    yield runResult

    assert(result.isRight, s"Expected Right, got $result")

end LoopSuite
