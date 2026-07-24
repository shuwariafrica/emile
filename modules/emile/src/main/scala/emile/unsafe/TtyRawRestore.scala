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
package emile.unsafe

import java.util.concurrent.atomic.AtomicBoolean
import scala.scalanative.libc.signal as clib
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.signal as posix
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** The two non-Resource legs of a raw tty's crash-restore contract, and the process-wide single-raw
  * guard they rest on. Only one tty may hold raw mode at a time (`uv_tty_reset_mode` restores a
  * single process-global tty), so [[install]] takes the guard and returns false if it is already
  * held; [[uninstall]] releases it. While held, two death modes the Resource release cannot cover
  * are handled: the five fatal signals (their handlers run the async-signal-safe
  * `uv_tty_reset_mode` then chain to the prior disposition and re-raise), and a non-signal hard
  * exit (a shutdown hook runs `uv_tty_reset_mode` when the guard is held). Neither touches a signal
  * a caller owns through [[emile.Signal$ Signal]]; orderly exit is the Resource release's job.
  */
@scala.annotation.internal.sharable
private[emile] object TtyRawRestore:

  // The process-wide single-raw guard: the crash-restore contract is only honest with one raw tty. Its
  // held state also gates the shutdown hook, which restores the terminal only while a raw window is open.
  private val active = new AtomicBoolean(false)

  // The shutdown hook is registered once, on the first raw window, and left for the process lifetime; it
  // reads `active` to know whether a restore is due, so no per-window add/remove (and no Thread held in
  // this module) is needed.
  private val hookInstalled = new AtomicBoolean(false)

  private inline val Count = 5

  // FFI: native signal-number and saved-disposition tables the async-signal-safe handler reads
  // (no Scala heap), raw while/var loops, and OOM throws.
  // scalafix:off DisableSyntax

  // The five fatal signals whose default action terminates the process, held in native memory so the
  // signal handler reaches for no Scala heap object.
  private val sigNums: Ptr[CInt] =
    val p = stdlib.malloc(Count.toUInt * sizeof[CInt])
    if p == null then throw new OutOfMemoryError("emile: tty signal-number table allocation failed")
    p.asInstanceOf[Ptr[CInt]]

  // The prior disposition of each signal, captured at install so the handler can chain to it.
  private val saved: Ptr[CFuncPtr1[CInt, Unit]] =
    val p = stdlib.malloc(Count.toUInt * sizeof[Ptr[Byte]])
    if p == null then throw new OutOfMemoryError("emile: tty signal-handler table allocation failed")
    p.asInstanceOf[Ptr[CFuncPtr1[CInt, Unit]]]

  sigNums(0) = clib.SIGSEGV
  sigNums(1) = posix.SIGBUS
  sigNums(2) = clib.SIGILL
  sigNums(3) = clib.SIGFPE
  sigNums(4) = clib.SIGABRT

  // Async-signal-safe: restores the terminal, then chains to the prior disposition and re-raises so
  // the original fatal action (core dump, crash reporter) still occurs. Every operation - the extern
  // uv_tty_reset_mode (verified async-signal-safe), the native-memory reads, signal, and raise - is
  // async-signal-safe and allocates nothing.
  private val onFatalSignal: CFuncPtr1[CInt, Unit] = (signum: CInt) =>
    LibUV.uv_tty_reset_mode(): Unit
    val idx = indexOf(signum)
    if idx >= 0 then clib.signal(signum, saved(idx)): Unit
    clib.raise(signum): Unit

  /** Take the single-raw guard and, if taken, install both non-Resource legs: the fatal-signal
    * handlers (saving each prior disposition) and - on the first ever call - the shutdown hook.
    * Returns false without touching either if a raw tty already holds the guard.
    */
  def install(): Boolean =
    if !active.compareAndSet(false, true) then false
    else
      var i = 0
      while i < Count do
        saved(i) = clib.signal(sigNums(i), onFatalSignal)
        i += 1
      ensureShutdownHook()
      true

  /** Restore the fatal-signal handlers and release the guard - the orderly counterpart to
    * [[install]], run from the raw Resource's release once the terminal is back to cooked mode. The
    * shutdown hook stays registered but goes dormant, as the guard it reads is now released.
    */
  def uninstall(): Unit =
    var i = 0
    while i < Count do
      clib.signal(sigNums(i), saved(i)): Unit
      i += 1
    active.set(false)

  private def ensureShutdownHook(): Unit =
    if hookInstalled.compareAndSet(false, true) then
      java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => restoreOnHardExit(), "emile-tty-raw-restore"))

  /** Restore the terminal on a non-signal hard exit, but only while a raw window is open - the
    * shutdown hook's body.
    */
  private[emile] def restoreOnHardExit(): Unit =
    if active.get() then LibUV.uv_tty_reset_mode(): Unit

  // Async-signal-safe index of `signum` in the native table (a bounded scan, no allocation), or -1.
  private def indexOf(signum: Int): Int =
    var found = -1
    var i = 0
    while i < Count do
      if sigNums(i) == signum then found = i
      i += 1
    found

  // scalafix:on DisableSyntax

end TtyRawRestore
