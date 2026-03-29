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

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import munit.FunSuite

import emile.Timeout

/** Poller tests link against the real libuv runtime to validate idle/interrupt semantics. */
class PollerSuite extends FunSuite:

  test("poll returns Idle on empty loop"):
    val poller = Poller().toOption.get
    try
      val result = poller.poll(0)
      assertEquals(result, PollResult.Idle)
    finally poller.close()

  test("poll returns Complete when handles are active, then Idle after they drain"):
    val poller = Poller().toOption.get
    val loop = poller.loop
    val timer = Timer.init(loop).toOption.get

    // Schedule a short timer to keep the loop active
    val started = timer.start(Timeout.millis(20), Timeout.Zero)(() => ())
    assert(started.isRight, s"timer start failed: $started")

    // With an active timer, a non-blocking poll should report Complete (loop alive)
    val first = poller.poll(0)
    assertEquals(first, PollResult.Complete)

    // After running until the timer fires, the loop should become idle
    val second = poller.poll(-1L)
    assertEquals(second, PollResult.Idle)

    val _ = timer.closeSync
    poller.close()

  test("poll default returns Idle on empty loop without hanging"):
    val poller = Poller().toOption.get
    val ref = new AtomicReference[PollResult]()
    val latch = new CountDownLatch(1)

    val t = new Thread(
      new Runnable:
        def run(): Unit =
          ref.set(poller.poll(-1L))
          latch.countDown()
    )
    t.start()

    val done = latch.await(500, java.util.concurrent.TimeUnit.MILLISECONDS)
    assert(done, "poll default should return promptly when loop is empty")
    assertEquals(ref.get(), PollResult.Idle)
    poller.close()

  test("interrupt wakes a blocking poll"):
    val poller = Poller().toOption.get
    val loop = poller.loop
    val timer = Timer.init(loop).toOption.get
    val start = timer.start(Timeout.millis(500), Timeout.Zero)(() => ())
    assert(start.isRight, s"timer start failed: $start")
    val ref = new AtomicReference[PollResult]()
    val latch = new CountDownLatch(1)

    val t = new Thread(
      new Runnable:
        def run(): Unit =
          ref.set(poller.poll(-1L))
          latch.countDown()
    )
    t.start()
    Thread.sleep(10) // allow poller to enter uv_run
    poller.interrupt()

    val done = latch.await(2, java.util.concurrent.TimeUnit.SECONDS)
    assert(done, "poll thread did not finish in time")
    assertEquals(ref.get(), PollResult.Interrupted)
    val _ = timer.closeSync
    poller.close()

  test("stop request is treated as Interrupted"):
    val poller = Poller().toOption.get
    val loop = poller.loop
    val timer = Timer.init(loop).toOption.get
    val start = timer.start(Timeout.millis(500), Timeout.Zero)(() => ())
    assert(start.isRight, s"timer start failed: $start")
    val ref = new AtomicReference[PollResult]()
    val latch = new CountDownLatch(1)

    val t = new Thread(
      new Runnable:
        def run(): Unit =
          ref.set(poller.poll(-1L))
          latch.countDown()
    )
    t.start()
    Thread.sleep(10)
    poller.stop()

    val done = latch.await(2, java.util.concurrent.TimeUnit.SECONDS)
    assert(done, "poll thread did not finish in time")
    assertEquals(ref.get(), PollResult.Interrupted)
    val _ = timer.closeSync
    poller.close()

  test("stop with no handles returns Interrupted immediately"):
    val poller = Poller().toOption.get
    poller.stop()
    val result = poller.poll(-1L)
    assertEquals(result, PollResult.Interrupted)
    poller.close()

  test("interrupt flag set before poll returns Interrupted"):
    val poller = Poller().toOption.get
    poller.interrupt()
    val result = poller.poll(-1L)
    assertEquals(result, PollResult.Interrupted)
    poller.close()

  test("poll completes after processing a timer callback"):
    val poller = Poller().toOption.get
    val loop = poller.loop
    val timer = Timer.init(loop).toOption.get

    val _ = timer.start(Timeout.millis(10), Timeout.Zero)(() => ())
    // One-shot timer fires during UV_RUN_ONCE then becomes inactive;
    // uv_run returns 0 when no active/referenced handles remain
    val result = poller.poll(-1L)
    assertEquals(result, PollResult.Idle)

    val _ = timer.closeSync
    // Drain close callback
    val _ = poller.poll(0L)
    poller.close()

end PollerSuite
