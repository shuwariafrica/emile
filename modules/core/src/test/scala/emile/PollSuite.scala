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

import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import munit.FunSuite

/** Tests for Poll handle operations.
  *
  * These tests link to and execute the real libuv library. We use pipes for testing as they're
  * portable across Unix systems.
  */
class PollSuite extends FunSuite:
// scalafix:off

  /** Helper to create a pipe and run test code with automatic cleanup. */
  private def withPipe[A](test: (CInt, CInt) => A): A =
    Zone:
      val pipeFds = stackalloc[CInt](2)
      val pipeResult = unistd.pipe(pipeFds)
      assertEquals(pipeResult, 0, "pipe() should succeed")
      val readFd = pipeFds(0)
      val writeFd = pipeFds(1)
      try test(readFd, writeFd)
      finally
        val _ = unistd.close(readFd)
        val _ = unistd.close(writeFd)

  test("Poll.init creates a valid poll handle"):
    withPipe { (readFd, _) =>
      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        _ = poll.close
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield ()

      assert(result.isRight, s"Expected Right, got $result")
    }

  test("Poll detects readable event"):
    withPipe { (readFd, writeFd) =>
      var readableDetected = false
      var pollRef: Poll[Open] = null.asInstanceOf[Poll[Open]]

      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        _ = pollRef = poll
        _ <- poll.start(PollEvent.Readable) { (status, events) =>
               if status >= 0 && events.contains(PollEvent.Readable) then
                 readableDetected = true
                 val _ = pollRef.stop
                 val _ = pollRef.close
             }
        // Write to pipe to make read end readable
        _ = Zone {
              val msg = stackalloc[Byte](1)
              !msg = 'x'.toByte
              val _ = unistd.write(writeFd, msg, 1.toCSize)
            }
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield ()

      assert(result.isRight, s"Expected Right, got $result")
      assert(readableDetected, "Should have detected readable event")
    }

  test("Poll detects writable event"):
    withPipe { (_, writeFd) =>
      var writableDetected = false
      var pollRef: Poll[Open] = null.asInstanceOf[Poll[Open]]

      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, writeFd)
        _ = pollRef = poll
        _ <- poll.start(PollEvent.Writable) { (status, events) =>
               if status >= 0 && events.contains(PollEvent.Writable) then
                 writableDetected = true
                 val _ = pollRef.stop
                 val _ = pollRef.close
             }
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield ()

      assert(result.isRight, s"Expected Right, got $result")
      assert(writableDetected, "Should have detected writable event (pipe write end)")
    }

  test("Poll.stop prevents further callbacks"):
    withPipe { (readFd, writeFd) =>
      var callbackCount = 0

      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        _ <- poll.start(PollEvent.Readable) { (_, _) =>
               callbackCount += 1
             }
        _ <- poll.stop
        // Write to pipe - but callback should not fire since we stopped
        _ = Zone {
              val msg = stackalloc[Byte](1)
              !msg = 'x'.toByte
              val _ = unistd.write(writeFd, msg, 1.toCSize)
            }
        _ <- loop.run(RunMode.NoWait)
        _ = poll.close
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield ()

      assert(result.isRight, s"Expected Right, got $result")
      assertEquals(callbackCount, 0, "Stopped poll should not invoke callback")
    }

  test("Poll Handle operations work"):
    withPipe { (readFd, _) =>
      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        isActive1 = poll.isActive
        _ <- poll.start(PollEvent.Readable)((_, _) => ())
        isActive2 = poll.isActive
        hasRef1 = poll.hasRef
        _ = poll.unref
        hasRef2 = poll.hasRef
        _ = poll.ref
        hasRef3 = poll.hasRef
        handleType = poll.handleType
        _ <- poll.stop
        _ = poll.close
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield (isActive1, isActive2, hasRef1, hasRef2, hasRef3, handleType)

      assert(result.isRight, s"Expected Right, got $result")
      result.foreach { case (isActive1, isActive2, hasRef1, hasRef2, hasRef3, handleType) =>
        assert(!isActive1, "Poll should not be active before start")
        assert(isActive2, "Poll should be active after start")
        assert(hasRef1, "Poll should be referenced by default")
        assert(!hasRef2, "Poll should not be referenced after unref")
        assert(hasRef3, "Poll should be referenced after ref")
        assertEquals(handleType, HandleType.Poll)
      }
    }

  test("restarting poll only fires the last registered callback"):
    withPipe { (readFd, writeFd) =>
      var fired1 = false
      var fired2 = false
      var fired3 = false
      var pollRef: Poll[Open] = null.asInstanceOf[Poll[Open]]

      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        _ = pollRef = poll
        // Start poll multiple times - only the last callback should fire
        _ <- poll.start(PollEvent.Readable) { (_, _) =>
               fired1 = true
               val _ = pollRef.stop
               val _ = pollRef.close
             }
        _ <- poll.start(PollEvent.Readable) { (_, _) =>
               fired2 = true
               val _ = pollRef.stop
               val _ = pollRef.close
             }
        _ <- poll.start(PollEvent.Readable) { (_, _) =>
               fired3 = true
               val _ = pollRef.stop
               val _ = pollRef.close
             }
        // Write to pipe to trigger readable event
        _ = Zone {
              val msg = stackalloc[Byte](1)
              !msg = 'x'.toByte
              val _ = unistd.write(writeFd, msg, 1.toCSize)
            }
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield ()

      assert(result.isRight, s"Expected Right, got $result")
      assert(!fired1, "First callback should not fire after being replaced")
      assert(!fired2, "Second callback should not fire after being replaced")
      assert(fired3, "Only the last registered callback should fire")
    }

  test("PollEvent.fromLibuv converts bitmask correctly"):
    assertEquals(PollEvent.fromLibuv(0), Set.empty[PollEvent])
    assertEquals(PollEvent.fromLibuv(1), Set(PollEvent.Readable))
    assertEquals(PollEvent.fromLibuv(2), Set(PollEvent.Writable))
    assertEquals(PollEvent.fromLibuv(3), Set(PollEvent.Readable, PollEvent.Writable))
    assertEquals(PollEvent.fromLibuv(4), Set(PollEvent.Disconnect))
    assertEquals(PollEvent.fromLibuv(8), Set(PollEvent.Prioritized))
    assertEquals(PollEvent.fromLibuv(15), Set(PollEvent.Readable, PollEvent.Writable, PollEvent.Disconnect, PollEvent.Prioritized))

  test("PollEvent.combine creates correct bitmask"):
    assertEquals(PollEvent.combine(), 0)
    assertEquals(PollEvent.combine(PollEvent.Readable), 1)
    assertEquals(PollEvent.combine(PollEvent.Writable), 2)
    assertEquals(PollEvent.combine(PollEvent.Readable, PollEvent.Writable), 3)
    assertEquals(PollEvent.combine(PollEvent.Readable, PollEvent.Writable, PollEvent.Disconnect), 7)

  // ===========================================================================
  // Lifecycle Safety Tests
  //
  // These tests verify poll handle lifecycle, particularly the stop/close
  // interaction with callbacks. Poll handles don't have global state like
  // signals, but still need proper cleanup of callbacks and file descriptors.
  // ===========================================================================

  test("Poll.closeAsync fires callback after handle is fully closed"):
    withPipe { (readFd, _) =>
      var closeCallbackFired = false

      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        _ <- poll.start(PollEvent.Readable)((_, _) => ())
        // Stop before close (defensive pattern matching Signal/Timer)
        _ <- poll.stop
        _ = poll.closeAsync(_ => closeCallbackFired = true)
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield ()

      assert(result.isRight, s"Expected Right, got $result")
      assert(closeCallbackFired, "Close callback must fire")
    }

  test("Rapid poll create/stop/close does not corrupt state"):
    // Stress test - rapidly create, start, stop, and close polls
    val iterations = 20
    var closeCount = 0

    val result = for
      loop <- Loop.create
      _ <- (0 until iterations).foldLeft(Right(()): Either[EmileError, Unit]) { (acc, _) =>
             acc.flatMap { _ =>
               withPipe { (readFd, _) =>
                 for
                   poll <- Poll.init(loop, readFd)
                   _ <- poll.start(PollEvent.Readable)((_, _) => ())
                   _ <- poll.stop
                   _ = poll.closeAsync(_ => closeCount += 1)
                   _ <- loop.run(RunMode.NoWait)
                 yield ()
               }
             }
           }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield closeCount

    assert(result.isRight, s"Expected Right, got $result")
    result.foreach(count => assertEquals(count, iterations, s"All $iterations close callbacks should fire"))

  test("Poll sequential fd reuse works correctly"):
    // Tests that closing a poll on an fd allows creating a new poll on a new fd
    withPipe { (readFd1, writeFd1) =>
      withPipe { (readFd2, writeFd2) =>
        var callback1Fired = false
        var callback2Fired = false
        var poll1Ref: Poll[Open] = null.asInstanceOf[Poll[Open]]
        var poll2Ref: Poll[Open] = null.asInstanceOf[Poll[Open]]

        val result = for
          loop <- Loop.create
          // First poll
          poll1 <- Poll.init(loop, readFd1)
          _ = poll1Ref = poll1
          _ <- poll1.start(PollEvent.Readable) { (_, _) =>
                 callback1Fired = true
                 val _ = poll1Ref.stop
                 val _ = poll1Ref.close
               }
          _ = Zone {
                val msg = stackalloc[Byte](1)
                !msg = 'a'.toByte
                val _ = unistd.write(writeFd1, msg, 1.toCSize)
              }
          _ <- loop.run(RunMode.Default)
          // Second poll on different fd
          poll2 <- Poll.init(loop, readFd2)
          _ = poll2Ref = poll2
          _ <- poll2.start(PollEvent.Readable) { (_, _) =>
                 callback2Fired = true
                 val _ = poll2Ref.stop
                 val _ = poll2Ref.close
               }
          _ = Zone {
                val msg = stackalloc[Byte](1)
                !msg = 'b'.toByte
                val _ = unistd.write(writeFd2, msg, 1.toCSize)
              }
          _ <- loop.run(RunMode.Default)
          _ <- loop.close
        yield ()

        assert(result.isRight, s"Expected Right, got $result")
        assert(callback1Fired, "First poll callback should fire")
        assert(callback2Fired, "Second poll callback should fire")
      }
    }

end PollSuite
