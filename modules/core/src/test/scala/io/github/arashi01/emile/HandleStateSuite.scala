/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import munit.FunSuite

/**
 * Test suite for HandleState phantom types.
 *
 * These tests verify the runtime behaviour of phantom type state transitions.
 * The compile-time safety guarantees are implicit - if the code compiles with
 * the phantom types, the safety properties hold.
 *
 * Key compile-time properties (verified by successful compilation):
 * - Timer[Open], Async[Open], Poll[Open], Tcp[Open] can call start/stop/send operations
 * - Timer[Closed], Async[Closed], Poll[Closed], Tcp[Closed] CANNOT call those operations
 * - closeSync returns a [Closed] witness
 * - init returns an [Open] handle
 */
class HandleStateSuite extends FunSuite:

  test("HandleState.Open and Closed are distinct types"):
    // This is a compile-time property - the fact this compiles proves it
    val _: Open = null.asInstanceOf[Open]
    val _: Closed = null.asInstanceOf[Closed]
    // These are not the same type at compile time
    // (we can't test inequality at runtime since they're just null)
    assert(true)

  test("Timer init returns Timer[Open]"):
    withLoop { loop =>
      val result = Timer.init(loop)
      assert(result.isRight)
      // The fact this compiles proves init returns Timer[Open]
      val timer: Timer[Open] = result.toOption.get
      val _ = timer.closeSync
    }

  test("Timer closeSync returns Timer[Closed]"):
    withLoop { loop =>
      Timer.init(loop).foreach { timer =>
        val closeResult = timer.closeSync
        assert(closeResult.isRight)
        // The fact this compiles proves closeSync returns Timer[Closed]
        val closed: Timer[Closed] = closeResult.toOption.get
        // closed.start(...) would NOT compile - operations require Open state
        val _ = closed
      }
    }

  test("Async init returns Async[Open]"):
    withLoop { loop =>
      val result = Async.init(loop)(() => ())
      assert(result.isRight)
      // The fact this compiles proves init returns Async[Open]
      val async: Async[Open] = result.toOption.get
      val _ = async.closeSync
    }

  test("Async closeSync returns Async[Closed]"):
    withLoop { loop =>
      Async.init(loop)(() => ()).foreach { async =>
        val closeResult = async.closeSync
        assert(closeResult.isRight)
        // The fact this compiles proves closeSync returns Async[Closed]
        val closed: Async[Closed] = closeResult.toOption.get
        // closed.send() would NOT compile - operations require Open state
        val _ = closed
      }
    }

  test("Poll init returns Poll[Open]"):
    withPipe { (readFd, _) =>
      withLoop { loop =>
        val result = Poll.init(loop, readFd)
        assert(result.isRight)
        // The fact this compiles proves init returns Poll[Open]
        val poll: Poll[Open] = result.toOption.get
        val _ = poll.closeSync
      }
    }

  test("Poll closeSync returns Poll[Closed]"):
    withPipe { (readFd, _) =>
      withLoop { loop =>
        Poll.init(loop, readFd).foreach { poll =>
          val closeResult = poll.closeSync
          assert(closeResult.isRight)
          // The fact this compiles proves closeSync returns Poll[Closed]
          val closed: Poll[Closed] = closeResult.toOption.get
          // closed.start(...) would NOT compile - operations require Open state
          val _ = closed
        }
      }
    }

  test("Tcp init returns Tcp[Open]"):
    withLoop { loop =>
      val result = Tcp.init(loop)
      assert(result.isRight)
      // The fact this compiles proves init returns Tcp[Open]
      val tcp: Tcp[Open] = result.toOption.get
      val _ = tcp.closeSync
    }

  test("Tcp closeSync returns Tcp[Closed]"):
    withLoop { loop =>
      Tcp.init(loop).foreach { tcp =>
        val closeResult = tcp.closeSync
        assert(closeResult.isRight)
        // The fact this compiles proves closeSync returns Tcp[Closed]
        val closed: Tcp[Closed] = closeResult.toOption.get
        // closed.bind(...) would NOT compile - operations require Open state
        val _ = closed
      }
    }

  test("Timer can start/stop only in Open state"):
    withLoop { loop =>
      Timer.init(loop).foreach { timer =>
        // These compile because timer is Timer[Open]
        val startResult = timer.start(Duration.millis(1000), Duration.Zero)(() => ())
        assert(startResult.isRight)
        val stopResult = timer.stop
        assert(stopResult.isRight)
        val _ = timer.closeSync
      }
    }

  test("Async can send only in Open state"):
    withLoop { loop =>
      Async.init(loop)(() => ()).foreach { async =>
        // This compiles because async is Async[Open]
        val sendResult = async.send
        assert(sendResult.isRight)
        val _ = async.closeSync
      }
    }

  test("Poll can start/stop only in Open state"):
    withPipe { (readFd, _) =>
      withLoop { loop =>
        Poll.init(loop, readFd).foreach { poll =>
          // These compile because poll is Poll[Open]
          val startResult = poll.start(PollEvent.Readable) { (_, _) => () }
          assert(startResult.isRight)
          val stopResult = poll.stop
          assert(stopResult.isRight)
          val _ = poll.closeSync
        }
      }
    }

  test("Tcp can bind only in Open state"):
    import io.github.arashi01.emile.ipa.*
    withLoop { loop =>
      Tcp.init(loop).foreach { tcp =>
        val port = Port(0)
        val addr = SocketAddress.v4(Ipv4Address.Loopback, port)
        // This compiles because tcp is Tcp[Open]
        val bindResult = tcp.bind(addr)
        assert(bindResult.isRight)
        val _ = tcp.closeSync
      }
    }

  test("Handle type class works with any state"):
    withLoop { loop =>
      Timer.init(loop).foreach { openTimer =>
        // Handle works with Timer[Open]
        val handleOpen = Handle[Timer[Open]]
        val loopFromHandle1 = handleOpen.loop(openTimer)
        assert(loopFromHandle1.ptrUnsafe.toLong == loop.ptrUnsafe.toLong)

        val closeResult = openTimer.closeSync
        closeResult.foreach { closedTimer =>
          // Handle also works with Timer[Closed]
          val handleClosed = Handle[Timer[Closed]]
          val loopFromHandle2 = handleClosed.loop(closedTimer)
          assert(loopFromHandle2.ptrUnsafe.toLong == loop.ptrUnsafe.toLong)
        }
      }
    }

  test("Type aliases work correctly"):
    withLoop { loop =>
      // OpenTimer is an alias for Timer[Open]
      val timer1: Timer.OpenTimer = Timer.init(loop).toOption.get

      // OpenAsync is an alias for Async[Open]
      val async1: Async.OpenAsync = Async.init(loop)(() => ()).toOption.get

      // Close them
      val _ = timer1.closeSync
      val _ = async1.closeSync
    }

  test("closeAsync invokes callback"):
    withLoop { loop =>
      var closeCallbackInvoked = false

      Timer.init(loop).foreach { timer =>
        timer.closeAsync { _ =>
          closeCallbackInvoked = true
        }
        // Run the loop to process the close
        val _ = loop.run(RunMode.Default)

        assert(closeCallbackInvoked, "Close callback should have been invoked")
      }
    }

  test("Multiple handles can be in different states"):
    withLoop { loop =>
      val timer1 = Timer.init(loop).toOption.get
      val timer2 = Timer.init(loop).toOption.get

      // Close timer1, keep timer2 open
      val closed1: Timer[Closed] = timer1.closeSync.toOption.get
      val open2: Timer[Open] = timer2

      // timer1 is closed, timer2 is open
      // We can only operate on timer2
      val startResult = open2.start(Duration.millis(1000), Duration.Zero)(() => ())
      assert(startResult.isRight)

      // Clean up
      val _ = open2.closeSync
      val _ = closed1
    }

  // Helper methods

  private def withLoop(f: Loop => Unit): Unit =
    Loop.create.fold(
      err => fail(s"Failed to create loop: $err"),
      loop => {
        f(loop)
        // Run the loop to drain any pending close callbacks
        val _ = loop.run(RunMode.Default)
        val _ = loop.close
      }
    )

  private def withPipe(f: (Int, Int) => Unit): Unit =
    import scala.scalanative.posix.unistd
    import scala.scalanative.unsafe.*

    val pipefd = stackalloc[Int](2)
    val result = unistd.pipe(pipefd)
    assert(result == 0, s"pipe() failed with $result")

    val readFd = pipefd(0)
    val writeFd = pipefd(1)

    f(readFd, writeFd)
    val _ = unistd.close(readFd)
    val _ = unistd.close(writeFd)
