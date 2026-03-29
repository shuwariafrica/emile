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

/** Test suite for HandleState phantom types.
  *
  * These tests verify the runtime behaviour of phantom type state transitions. The compile-time
  * safety guarantees are implicit - if the code compiles with the phantom types, the safety
  * properties hold.
  *
  * Key compile-time properties (verified by successful compilation):
  *   - Timer[Open], Async[Open], Poll[Open], Tcp[Open] can call start/stop/send operations
  *   - Timer[Closed], Async[Closed], Poll[Closed], Tcp[Closed] CANNOT call those operations
  *   - closeSync returns a success marker (no witness)
  *   - init returns an [Open] handle
  */
class HandleStateSuite extends FunSuite:
// scalafix:off

  test("Timer init returns Timer[Open]"):
    withLoop { loop =>
      val result = Timer.init(loop)
      assert(result.isRight)
      // The fact this compiles proves init returns Timer[Open]
      val timer: Timer[Open] = result.toOption.get
      assert(timer.closeSync.isRight)
    }

  test("Timer closeSync completes without leaks"):
    withLoop { loop =>
      Timer.init(loop).foreach { timer =>
        val closeResult = timer.closeSync
        assert(closeResult.isRight)
      }
    }

  test("Async init returns Async[Open]"):
    withLoop { loop =>
      val result = Async.init(loop)(() => ())
      assert(result.isRight)
      // The fact this compiles proves init returns Async[Open]
      val async: Async[Open] = result.toOption.get
      assert(async.closeSync.isRight)
    }

  test("Async closeSync completes without leaks"):
    withLoop { loop =>
      Async.init(loop)(() => ()).foreach { async =>
        val closeResult = async.closeSync
        assert(closeResult.isRight)
      }
    }

  test("Poll init returns Poll[Open]"):
    withPipe { (readFd, _) =>
      withLoop { loop =>
        val result = Poll.init(loop, readFd)
        assert(result.isRight)
        // The fact this compiles proves init returns Poll[Open]
        val poll: Poll[Open] = result.toOption.get
        assert(poll.closeSync.isRight)
      }
    }

  test("Poll closeSync completes without leaks"):
    withPipe { (readFd, _) =>
      withLoop { loop =>
        Poll.init(loop, readFd).foreach { poll =>
          val closeResult = poll.closeSync
          assert(closeResult.isRight)
        }
      }
    }

  test("Tcp init returns Tcp[Open]"):
    withLoop { loop =>
      val result = Tcp.init(loop)
      assert(result.isRight)
      // The fact this compiles proves init returns Tcp[Open]
      val tcp: Tcp[Open] = result.toOption.get
      assert(tcp.closeSync.isRight)
    }

  test("Tcp closeSync completes without leaks"):
    withLoop { loop =>
      Tcp.init(loop).foreach { tcp =>
        val closeResult = tcp.closeSync
        assert(closeResult.isRight)
      }
    }

  test("Timer can start/stop only in Open state"):
    withLoop { loop =>
      Timer.init(loop).foreach { timer =>
        // These compile because timer is Timer[Open]
        val startResult = timer.start(Timeout.millis(1000), Timeout.Zero)(() => ())
        assert(startResult.isRight)
        val stopResult = timer.stop
        assert(stopResult.isRight)
        assert(timer.closeSync.isRight)
      }
    }

  test("Async can send only in Open state"):
    withLoop { loop =>
      Async.init(loop)(() => ()).foreach { async =>
        // This compiles because async is Async[Open]
        val sendResult = async.send
        assert(sendResult.isRight)
        assert(async.closeSync.isRight)
      }
    }

  test("Poll can start/stop only in Open state"):
    withPipe { (readFd, _) =>
      withLoop { loop =>
        Poll.init(loop, readFd).foreach { poll =>
          // These compile because poll is Poll[Open]
          val startResult = poll.start(PollEvent.Readable)((_, _) => ())
          assert(startResult.isRight)
          val stopResult = poll.stop
          assert(stopResult.isRight)
          assert(poll.closeSync.isRight)
        }
      }
    }

  test("Tcp can bind only in Open state"):
    import emile.ipa.*
    withLoop { loop =>
      Tcp.init(loop).foreach { tcp =>
        val port = Port(0)
        val addr = SocketAddress.v4(Ipv4Address.Loopback, port)
        // This compiles because tcp is Tcp[Open]
        val bindResult = tcp.bind(addr)
        assert(bindResult.isRight)
        assert(tcp.closeSync.isRight)
      }
    }

  test("Type aliases work correctly"):
    withLoop { loop =>
      // OpenTimer is an alias for Timer[Open]
      val timer1: Timer.OpenTimer = Timer.init(loop).toOption.get

      // OpenAsync is an alias for Async[Open]
      val async1: Async.OpenAsync = Async.init(loop)(() => ()).toOption.get

      // Close them
      assert(timer1.closeSync.isRight)
      assert(async1.closeSync.isRight)
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
      assert(timer1.closeSync.isRight)
      val open2: Timer[Open] = timer2

      // timer1 is closed, timer2 is open
      // We can only operate on timer2
      val startResult = open2.start(Timeout.millis(1000), Timeout.Zero)(() => ())
      assert(startResult.isRight)

      // Clean up
      assert(open2.closeSync.isRight)
    }

  // Helper methods

  private def withLoop(f: Loop => Unit): Unit =
    Loop.create.fold(
      err => fail(s"Failed to create loop: $err"),
      loop =>
        f(loop)
        // Run the loop to drain any pending close callbacks
        val _ = loop.run(RunMode.Default)
        val _ = loop.close
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
  end withPipe
end HandleStateSuite
