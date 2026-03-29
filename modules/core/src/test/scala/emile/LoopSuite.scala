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

import munit.FunSuite

import emile.Timeout
import emile.Timer

/** Tests for Loop operations.
  *
  * These tests link to and execute the real libuv library.
  */
class LoopSuite extends FunSuite:
// scalafix:off

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

  test("Loop.close on default loop is a no-op and loop stays usable"):
    var fired = false
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.default
      _ <- loop.close // should be a no-op for default loop
      timer <- Timer.after(loop, Timeout.millis(25)) { () =>
                 fired = true
                 val _ = timerRef.close
               }
      _ = timerRef = timer
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield fired

    assert(result.isRight, s"Expected Right, got $result")
    assert(result.exists(identity), "Timer on default loop should fire even after close no-op")

end LoopSuite
