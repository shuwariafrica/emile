/*
 * Copyright 2025, 2026 Ali Rashid
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

import scala.concurrent.duration.*
import scala.scalanative.libc.signal as clib
import scala.scalanative.posix.fcntl
import scala.scalanative.posix.stdlib as posixStdlib
import scala.scalanative.posix.sys.ioctl as ioctlLib
import scala.scalanative.posix.termios
import scala.scalanative.posix.termiosOps.*
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource

import emile.unsafe.TtyRawRestore

// A pseudo-terminal pair (posix_openpt) is the hermetic stand-in for the probe's `script -qec`
// harness. The fatal-signal restore leg has no row: a real fatal signal terminates the test process,
// and its handler runs the same async-signal-safe uv_tty_reset_mode the tested legs use.
final class TtySpec extends EmileSuite:

  test("guess and isTty classify a terminal, a pipe, and a file") {
    ptyResource.use { pty =>
      secondaryResource(pty).use { sec =>
        pipeResource.use { (readFd, _) =>
          fileResource.use { fileFd =>
            IO {
              assertEquals(Tty.guess(sec), FdKind.Tty)
              assert(Tty.isTty(sec))
              assertEquals(Tty.guess(readFd), FdKind.Pipe)
              assert(!Tty.isTty(readFd))
              assertEquals(Tty.guess(fileFd), FdKind.File)
            }
          }
        }
      }
    }
  }

  test("open rejects a non-terminal descriptor with InvalidArgument") {
    fileResource.use { fileFd =>
      Tty
        .open(fileFd)
        .use(_ => EffIO.succeed(()))
        .either
        .map {
          case Left(EmileError.IO.InvalidArgument(_)) => ()
          case other => fail(s"expected InvalidArgument, got: $other")
        }
        .timeout(5.seconds)
    }
  }

  test("raw mode round-trips, restoring cooked mode on release") {
    ptyResource.use { pty =>
      secondaryResource(pty).use { sec =>
        inspectResource(pty).use { inspect =>
          Tty
            .open(sec)
            .use { tty =>
              for
                _ <- EffIO.liftF(IO(assert(isCanonical(inspect), "terminal starts in canonical mode")))
                _ <- tty.raw.use(_ => EffIO.liftF(IO(assert(!isCanonical(inspect), "raw mode clears canonical mode"))))
                _ <- EffIO.liftF(IO(assert(isCanonical(inspect), "release restores canonical mode")))
              yield ()
            }
            .absolve
            .timeout(5.seconds)
        }
      }
    }
  }

  test("the shutdown-hook restore returns a held raw terminal to cooked mode") {
    ptyResource.use { pty =>
      secondaryResource(pty).use { sec =>
        inspectResource(pty).use { inspect =>
          Tty
            .open(sec)
            .use { tty =>
              tty.raw.use { _ =>
                EffIO.liftF(IO {
                  assert(!isCanonical(inspect), "raw mode is in force")
                  // The shutdown hook's body, run while the raw window is open, must restore the terminal.
                  TtyRawRestore.restoreOnHardExit()
                  assert(isCanonical(inspect), "the hard-exit restore returns the terminal to cooked mode")
                })
              }
            }
            .absolve
            .timeout(5.seconds)
        }
      }
    }
  }

  test("a second concurrent raw mode fails with ConflictingOperation") {
    ptyResource.use { pty =>
      secondaryResource(pty).use { sec =>
        Tty
          .open(sec)
          .use { tty =>
            tty.raw.use { _ =>
              EffIO.liftF(
                tty.raw
                  .use(_ => EffIO.succeed(()))
                  .either
                  .map {
                    case Left(EmileError.IO.ConflictingOperation) => ()
                    case other => fail(s"expected ConflictingOperation, got: $other")
                  }
              )
            }
          }
          .absolve
          .timeout(5.seconds)
      }
    }
  }

  test("size reports the window dimensions, and 0x0 on an unsized pseudo-terminal") {
    ptyResource.use { pty =>
      secondaryResource(pty).use { sec =>
        Tty
          .open(sec)
          .use { tty =>
            for
              initial <- tty.size
              _ <- EffIO.liftF(IO(setWinsize(pty.primary, 120, 40)))
              updated <- tty.size
            yield
              assertEquals(initial, WinSize(0, 0))
              assertEquals(updated, WinSize(120, 40))
          }
          .absolve
          .timeout(5.seconds)
      }
    }
  }

  test("resizes emits the current size, then a fresh reading on SIGWINCH") {
    ptyResource.use { pty =>
      IO(setWinsize(pty.primary, 80, 24)) *> secondaryResource(pty).use { sec =>
        Tty
          .open(sec)
          .use { tty =>
            val watch = tty.resizes.take(2).compile.toList.absolve
            // Once the stream has emitted the current size and subscribed for SIGWINCH, resize and raise it.
            val drive = IO.sleep(300.millis) *> IO(setWinsize(pty.primary, 132, 43)) *> IO(clib.raise(SIGWINCH): Unit)
            EffIO.liftF(watch.both(drive).map((sizes, _) => assertEquals(sizes, List(WinSize(80, 24), WinSize(132, 43)))))
          }
          .absolve
          .timeout(5.seconds)
      }
    }
  }

  test("write emits bytes to the terminal stream") {
    ptyResource.use { pty =>
      secondaryResource(pty).use { sec =>
        Tty
          .open(sec)
          .use { tty =>
            tty.raw.use { _ =>
              for
                _ <- tty.write(Slice.of("ping".getBytes("UTF-8"), 0, 4))
                received <- EffIO.liftF(IO.blocking(readFrom(pty.primary, 4)))
              yield assertEquals(received, "ping")
            }
          }
          .absolve
          .timeout(5.seconds)
      }
    }
  }

  // SIGWINCH is not in scala-native's POSIX signal bindings (it is not in POSIX.1); 28 is its Linux value.
  private inline val SIGWINCH = 28

  // Holds the master side of a pseudo-terminal pair and the path of its slave side.
  final private class Pty(val primary: Int, val secondaryPath: String)

  private def ptyResource: Resource[IO, Pty] =
    Resource.make(IO(openPty()))(pty => IO(unistd.close(pty.primary): Unit))

  // A slave-side descriptor for Tty.open; libuv reopens the terminal and duplicates over this number,
  // so the original still needs closing when the resource releases.
  private def secondaryResource(pty: Pty): Resource[IO, Int] =
    Resource.make(IO(openPath(pty.secondaryPath)))(fd => IO(unistd.close(fd): Unit))

  // A second, independent slave-side descriptor for termios inspection - device-level attributes are
  // shared with the descriptor libuv drives, so this observes raw / cooked transitions without racing it.
  private def inspectResource(pty: Pty): Resource[IO, Int] =
    Resource.make(IO(openPath(pty.secondaryPath)))(fd => IO(unistd.close(fd): Unit))

  private def pipeResource: Resource[IO, (Int, Int)] =
    Resource.make(IO(openPipe()))((readFd, writeFd) => IO(closePipe(readFd, writeFd)))

  private def fileResource: Resource[IO, Int] =
    Resource.make(IO(openDevNull()))(fd => IO(unistd.close(fd): Unit))

  // FFI: pseudo-terminal setup, winsize and termios structs, and raw reads over native pointers.
  // scalafix:off DisableSyntax

  private def openPty(): Pty =
    val primary = posixStdlib.posix_openpt(fcntl.O_RDWR | fcntl.O_NOCTTY)
    if primary < 0 then throw new RuntimeException("posix_openpt failed")
    if posixStdlib.grantpt(primary) < 0 || posixStdlib.unlockpt(primary) < 0 then throw new RuntimeException("grantpt/unlockpt failed")
    new Pty(primary, fromCString(posixStdlib.ptsname(primary)))

  private def openPath(path: String): Int =
    Zone.acquire(implicit z => fcntl.open(toCString(path), fcntl.O_RDWR | fcntl.O_NOCTTY))

  private def openDevNull(): Int = fcntl.open(c"/dev/null", fcntl.O_RDONLY)

  private def openPipe(): (Int, Int) =
    val fds = stackalloc[CInt](2)
    unistd.pipe(fds): Unit
    (fds(0), fds(1))

  private def closePipe(readFd: Int, writeFd: Int): Unit =
    unistd.close(readFd): Unit
    unistd.close(writeFd): Unit

  // struct winsize { unsigned short ws_row, ws_col, ws_xpixel, ws_ypixel; }; TIOCSWINSZ is Linux's value.
  private type Winsize = CStruct4[CUnsignedShort, CUnsignedShort, CUnsignedShort, CUnsignedShort]
  private inline val TIOCSWINSZ = 0x5414

  private def setWinsize(fd: Int, cols: Int, rows: Int): Unit =
    val ws = stackalloc[Winsize]()
    ws._1 = rows.toUShort
    ws._2 = cols.toUShort
    ws._3 = 0.toUShort
    ws._4 = 0.toUShort
    ioctlLib.ioctl(fd, TIOCSWINSZ, ws.asInstanceOf[Ptr[Byte]]): Unit

  private def isCanonical(fd: Int): Boolean =
    val tio = stackalloc[termios.termios]()
    termios.tcgetattr(fd, tio): Unit
    (tio.c_lflag.toLong & termios.ICANON.toLong) != 0L

  private def readFrom(fd: Int, count: Int): String =
    val buf = stackalloc[Byte](count)
    var total = 0
    while total < count do
      val n = unistd.read(fd, buf + total, (count - total).toCSize)
      if n <= 0 then total = count
      else total += n
    val bytes = new Array[Byte](count)
    var i = 0
    while i < count do
      bytes(i) = buf(i)
      i += 1
    new String(bytes, "UTF-8")

  // scalafix:on DisableSyntax

end TtySpec
