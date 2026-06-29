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
import scala.scalanative.posix.signal as posix
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.toRawPtr
import scala.scalanative.unsafe.CFuncPtr
import scala.scalanative.unsafe.CFuncPtr1
import scala.scalanative.unsafe.CInt

import cats.effect.IO

/** Covers the signal supervisor: a single delivery reaches every concurrent watcher (`SIGURG`,
  * whose default disposition is to be ignored), and the prior disposition is restored when the last
  * subscriber for a signal unsubscribes (`SIGUSR1`).
  */
final class SignalSupervisorSpec extends EmileSuite:

  test("a raised signal reaches every concurrent watcher") {
    val watch = Signal.watch(SignalNumber(posix.SIGURG)).head.compile.drain.absolve
    val raise = IO.sleep(250.millis) *> IO(clib.raise(posix.SIGURG): Unit)
    watch.both(watch).both(raise).timeout(5.seconds).void
  }

  // SIGUSR1 is unused by other specs. Its disposition is set to ignore before watching so the
  // assertion discriminates a true restore from libuv's plain stop (which leaves SIG_DFL) and from
  // never uninstalling (which leaves libuv's own handler).
  test("the prior disposition is restored when the last subscriber unsubscribes") {
    val signum = SignalNumber.SIGUSR1
    val sig = SignalNumber.unwrap(signum)
    val ignore = address(clib.SIG_IGN)
    val watch = Signal.watch(signum).compile.drain.absolve
    for
      _ <- IO(clib.signal(sig, clib.SIG_IGN): Unit)
      fibre <- watch.start
      _ <- IO.sleep(300.millis)
      during <- IO(currentHandler(sig))
      _ <- fibre.cancel
      after <- IO(currentHandler(sig))
      _ <- IO(clib.signal(sig, clib.SIG_DFL): Unit)
    yield
      assertNotEquals(during, ignore)
      assertEquals(after, ignore)
  }

  private def address(handler: CFuncPtr1[CInt, Unit]): Long =
    Intrinsics.castRawPtrToLong(toRawPtr(CFuncPtr.toPtr(handler)))

  // Reads the current handler by swapping it out and immediately back; the address alone discriminates
  // SIG_IGN from SIG_DFL and from libuv's handler, and the disposition is left unchanged.
  private def currentHandler(sig: CInt): Long =
    val previous = clib.signal(sig, clib.SIG_DFL)
    clib.signal(sig, previous): Unit
    address(previous)
end SignalSupervisorSpec
