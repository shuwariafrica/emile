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

// scalafix:off DisableSyntax.var, DisableSyntax.while; signal tests require mutable state and polling loops

import scala.scalanative.meta.LinktimeInfo.isWindows
import scala.scalanative.posix.signal.kill
import scala.scalanative.posix.unistd.getpid

import munit.FunSuite

/** Tests for SignalHandle operations using libuv's native signal API.
  *
  * Signal delivery tests use a drain loop that keeps running the event loop until the callback
  * fires. This prevents the race where `uv_signal_stop` deregisters the sigaction handler while a
  * signal is still pending, which would restore SIG_DFL and terminate the process.
  */
class SignalHandleSuite extends FunSuite:

  private def drainUntil(loop: Loop, condition: () => Boolean): Unit =
    var i = 0
    while !condition() && i < 100 do
      val _ = loop.run(RunMode.Once)
      i += 1

  // ===========================================================================
  // Handle Lifecycle Tests
  // ===========================================================================

  test("SignalHandle.init creates a valid handle"):
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.init(loop)
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield signal
    assert(result.isRight, s"Expected Right, got $result")

  test("SignalHandle.start registers callback without error"):
    assume(!isWindows, "Signal test requires Unix")
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.init(loop)
      _ <- signal.start(Signal.SIGUSR1)(() => ())
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")

  test("SignalHandle.stop stops watching without error"):
    assume(!isWindows, "Signal test requires Unix")
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.init(loop)
      _ <- signal.start(Signal.SIGUSR1)(() => ())
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")

  test("SignalHandle.watch convenience creates started handle"):
    assume(!isWindows, "Signal test requires Unix")
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => ())
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")

  test("SignalHandle.once creates one-shot handle"):
    assume(!isWindows, "Signal test requires Unix")
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.once(loop, Signal.SIGUSR1)(() => ())
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")

  test("SignalHandle.closeAsync closes handle asynchronously"):
    assume(!isWindows, "Signal test requires Unix")
    var closeCallbackFired = false
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.init(loop)
      _ = signal.closeAsync(_ => closeCallbackFired = true)
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")
    assert(closeCallbackFired, "Close callback should have fired")

  test("Multiple signal handles can watch the same signal"):
    assume(!isWindows, "Signal test requires Unix")
    val result = for
      loop <- Loop.create
      signal1 <- SignalHandle.init(loop)
      signal2 <- SignalHandle.init(loop)
      _ <- signal1.start(Signal.SIGUSR1)(() => ())
      _ <- signal2.start(Signal.SIGUSR1)(() => ())
      _ <- signal1.stop
      _ <- signal2.stop
      _ = signal1.close
      _ = signal2.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")

  // ===========================================================================
  // Signal Delivery Tests
  // ===========================================================================

  test("Signal callback is invoked when signal is received"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var callbackInvoked = false
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => callbackInvoked = true)
      _ = kill(getpid(), Signal.SIGUSR2)
      _ = drainUntil(loop, () => callbackInvoked)
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")
    assert(callbackInvoked, "Signal callback should have been invoked")

  test("Signal callback can be invoked multiple times"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var invocationCount = 0
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => invocationCount += 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocationCount >= 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocationCount >= 2)
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield invocationCount
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assert(count >= 2, s"Expected at least 2 invocations, got $count"))

  test("One-shot handler fires exactly once and auto-stops"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var invocationCount = 0
    var wasActiveBeforeSignal = false
    var wasActiveAfterSignal = false
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.once(loop, Signal.SIGUSR2)(() => invocationCount += 1)
      _ = wasActiveBeforeSignal = signal.isActive
      _ = kill(getpid(), Signal.SIGUSR2)
      _ = drainUntil(loop, () => invocationCount >= 1)
      _ = wasActiveAfterSignal = signal.isActive
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield invocationCount
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assertEquals(count, 1, "One-shot should fire exactly once"))
    assert(wasActiveBeforeSignal, "Handle should be active before signal")
    assert(!wasActiveAfterSignal, "One-shot handle should auto-stop after firing")

  test("Multiple handles receive the same signal"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var count1 = 0
    var count2 = 0
    val result = for
      loop <- Loop.create
      signal1 <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => count1 += 1)
      signal2 <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => count2 += 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => count1 >= 1 && count2 >= 1)
      _ <- signal1.stop
      _ <- signal2.stop
      _ = signal1.close
      _ = signal2.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")
    assert(count1 >= 1, s"First handler should have fired, count=$count1")
    assert(count2 >= 1, s"Second handler should have fired, count=$count2")

  test("Stopped handler does not receive signals"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var stoppedHandlerInvoked = false
    var catcherHandlerInvoked = false
    val result = for
      loop <- Loop.create
      stoppedSignal <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => stoppedHandlerInvoked = true)
      catcherSignal <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => catcherHandlerInvoked = true)
      _ <- stoppedSignal.stop
      _ = kill(getpid(), Signal.SIGUSR2)
      _ = drainUntil(loop, () => catcherHandlerInvoked)
      _ <- catcherSignal.stop
      _ = stoppedSignal.close
      _ = catcherSignal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")
    assert(!stoppedHandlerInvoked, "Stopped handler should not receive signals")
    assert(catcherHandlerInvoked, "Catcher handler should have received the signal")

  test("Handler can be restarted after stop"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var invocationCount = 0
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => invocationCount += 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocationCount >= 1)
      _ <- signal.stop
      _ <- signal.start(Signal.SIGUSR1)(() => invocationCount += 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocationCount >= 2)
      _ <- signal.stop
      _ = signal.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield invocationCount
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assert(count >= 2, s"Expected at least 2 invocations after restart, got $count"))

  test("Signal delivered via timer fires callback"):
    assume(!isWindows, "Signal delivery requires Unix/POSIX")
    var signalFired = false
    var timerFired = false
    val result = for
      loop <- Loop.create
      signal <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => signalFired = true)
      timer <- Timer.init(loop)
      _ <- timer.startOnce(Timeout.millis(10)) { () =>
             timerFired = true
             val _ = kill(getpid(), Signal.SIGUSR2)
           }
      _ = drainUntil(loop, () => signalFired)
      _ <- signal.stop
      _ = signal.close
      _ = timer.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")
    assert(timerFired, "Timer should have fired")
    assert(signalFired, "Signal callback should have fired")

  // ===========================================================================
  // Signal.isSupported Tests
  // ===========================================================================

  test("Signal.isSupported returns true for SIGINT"):
    assert(Signal.isSupported(Signal.SIGINT), "SIGINT is supported on all platforms")

  test("Signal.isSupported returns false for SIGKILL"):
    assert(!Signal.isSupported(Signal.SIGKILL), "SIGKILL cannot be caught")

  test("Signal.isSupported returns false for SIGSTOP"):
    assert(!Signal.isSupported(Signal.SIGSTOP), "SIGSTOP cannot be caught")

  test("Signal.isSupported on Unix returns true for SIGUSR1"):
    assume(!isWindows, "SIGUSR1 is Unix-only")
    assert(Signal.isSupported(Signal.SIGUSR1))

  test("Signal.isSupported on Unix returns true for SIGUSR2"):
    assume(!isWindows, "SIGUSR2 is Unix-only")
    assert(Signal.isSupported(Signal.SIGUSR2))

  test("Signal.isSupported on Windows returns true for SIGBREAK"):
    assume(isWindows, "SIGBREAK is Windows-specific")
    assert(Signal.isSupported(Signal.SIGBREAK))

  // ===========================================================================
  // Lifecycle Safety Tests
  // ===========================================================================

  test("Rapid create/stop/close cycle does not corrupt signal tree"):
    assume(!isWindows, "Signal test requires Unix")
    val iterations = 20
    val signals = List(Signal.SIGUSR1, Signal.SIGUSR2)
    val result = for
      loop <- Loop.create
      _ <- (0 until iterations).foldLeft(Right(()): Either[EmileError, Unit]) { (acc, i) =>
             acc.flatMap { _ =>
               val sig = signals(i % signals.size)
               for
                 handle <- SignalHandle.init(loop)
                 _ <- handle.start(sig)(() => ())
                 _ <- handle.stop
                 _ = handle.close
                 _ <- loop.run(RunMode.NoWait)
               yield ()
             }
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right after $iterations iterations, got $result")

  test("Sequential signal handle reuse maintains tree integrity"):
    assume(!isWindows, "Signal test requires Unix")
    var invocationCount = 0
    val result = for
      loop <- Loop.create
      handle1 <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => invocationCount += 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocationCount >= 1)
      _ <- handle1.stop
      _ = handle1.close
      _ <- loop.run(RunMode.NoWait)
      handle2 <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => invocationCount += 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocationCount >= 2)
      _ <- handle2.stop
      _ = handle2.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield invocationCount
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assertEquals(count, 2, "Both handles should have received signals"))

  test("Close with pending signal is handled safely"):
    assume(!isWindows, "Signal test requires Unix")
    var signalReceived = false
    var closeCallbackFired = false
    var catcherReceived = false
    val result = for
      loop <- Loop.create
      catcher <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => catcherReceived = true)
      handle <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => signalReceived = true)
      _ = kill(getpid(), Signal.SIGUSR2)
      _ = drainUntil(loop, () => signalReceived || catcherReceived)
      _ <- handle.stop
      _ = handle.closeAsync(_ => closeCallbackFired = true)
      _ <- catcher.stop
      _ = catcher.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()
    assert(result.isRight, s"Expected Right, got $result")
    assert(closeCallbackFired, "Close callback must fire")
    assert(signalReceived || catcherReceived, "Signal should be received by at least one handler")

  test("Multiple concurrent handles for same signal close cleanly"):
    assume(!isWindows, "Signal test requires Unix")
    val handleCount = 5
    val invocations = new java.util.concurrent.atomic.AtomicInteger(0)
    var closeCount = 0
    val result = for
      loop <- Loop.create
      handles <- (0 until handleCount).foldLeft(Right(List.empty[SignalHandle[Open]]): Either[EmileError, List[SignalHandle[Open]]]) {
                   (acc, _) =>
                     acc.flatMap { list =>
                       SignalHandle
                         .watch(loop, Signal.SIGUSR1)(() => invocations.incrementAndGet(): Unit)
                         .map(_ :: list)
                     }
                 }
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => invocations.get() >= handleCount)
      _ <- handles.foldLeft(Right(()): Either[EmileError, Unit]) { (acc, h) =>
             acc.flatMap { _ =>
               h.stop.flatMap { _ =>
                 h.closeAsync(_ => closeCount += 1)
                 Right(())
               }
             }
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield invocations.get()
    assert(result.isRight, s"Expected Right, got $result")
    assertEquals(closeCount, handleCount, s"All $handleCount close callbacks should fire")
    result.foreach(count => assert(count >= handleCount, s"Each handle should receive signal, got $count"))

  test("Alternating signals stress test"):
    assume(!isWindows, "Signal test requires Unix")
    val usr1Count = new java.util.concurrent.atomic.AtomicInteger(0)
    val usr2Count = new java.util.concurrent.atomic.AtomicInteger(0)
    val result = for
      loop <- Loop.create
      h1 <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => usr1Count.incrementAndGet(): Unit)
      h2 <- SignalHandle.watch(loop, Signal.SIGUSR2)(() => usr2Count.incrementAndGet(): Unit)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => usr1Count.get() >= 1)
      _ = kill(getpid(), Signal.SIGUSR2)
      _ = drainUntil(loop, () => usr2Count.get() >= 1)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => usr1Count.get() >= 2)
      _ <- h1.stop
      _ = h1.close
      _ <- loop.run(RunMode.NoWait)
      _ = kill(getpid(), Signal.SIGUSR2)
      _ = drainUntil(loop, () => usr2Count.get() >= 2)
      h1b <- SignalHandle.watch(loop, Signal.SIGUSR1)(() => usr1Count.incrementAndGet(): Unit)
      _ = kill(getpid(), Signal.SIGUSR1)
      _ = drainUntil(loop, () => usr1Count.get() >= 3)
      _ <- h1b.stop
      _ <- h2.stop
      _ = h1b.close
      _ = h2.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield (usr1Count.get(), usr2Count.get())
    assert(result.isRight, s"Expected Right, got $result")
    result.foreach { case (c1, c2) =>
      assert(c1 >= 3, s"SIGUSR1 should be received at least 3 times, got $c1")
      assert(c2 >= 2, s"SIGUSR2 should be received at least 2 times, got $c2")
    }

end SignalHandleSuite
