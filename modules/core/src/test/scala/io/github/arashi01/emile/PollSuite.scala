/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import munit.FunSuite
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/**
 * Tests for Poll handle operations.
 *
 * These tests link to and execute the real libuv library.
 * We use pipes for testing as they're portable across Unix systems.
 */
class PollSuite extends FunSuite:

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
    withPipe { (readFd, writeFd) =>
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
        _ = { pollRef = poll }
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
    withPipe { (readFd, writeFd) =>
      var writableDetected = false
      var pollRef: Poll[Open] = null.asInstanceOf[Poll[Open]]

      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, writeFd)
        _ = { pollRef = poll }
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
    withPipe { (readFd, writeFd) =>
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

  test("restarting poll does not leak callbacks"):
    import io.github.arashi01.emile.unsafe.CallbackRegistry

    withPipe { (readFd, writeFd) =>
      val result = for
        loop <- Loop.create
        poll <- Poll.init(loop, readFd)
        initialSize = CallbackRegistry.size
        _ <- poll.start(PollEvent.Readable)((_, _) => ())
        sizeAfterFirst = CallbackRegistry.size
        _ <- poll.start(PollEvent.Writable)((_, _) => ())
        sizeAfterSecond = CallbackRegistry.size
        _ <- poll.start(PollEvent.Readable, PollEvent.Writable)((_, _) => ())
        sizeAfterThird = CallbackRegistry.size
        _ <- poll.stop
        _ = poll.close
        _ <- loop.run(RunMode.Default)
        _ <- loop.close
      yield (initialSize, sizeAfterFirst, sizeAfterSecond, sizeAfterThird)

      assert(result.isRight, s"Expected Right, got $result")
      result.foreach { case (initial, first, second, third) =>
        assertEquals(first, initial + 1, "First start should add one callback")
        assertEquals(second, initial + 1, "Second start should replace, not add")
        assertEquals(third, initial + 1, "Third start should replace, not add")
      }
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

end PollSuite
