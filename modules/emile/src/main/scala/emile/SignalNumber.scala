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

import scala.scalanative.posix.signal as posix

import boilerplate.OpaqueType

/** A validated POSIX signal number, constrained to the range `1..64`. Constructed through
  * [[SignalNumber$ SignalNumber]].
  */
opaque type SignalNumber = Int

/** Factory, validation, and curated constants for [[SignalNumber]]. */
object SignalNumber extends OpaqueType[SignalNumber, Int], OpaqueType.Eq[SignalNumber]:

  /** Raised when a signal number falls outside `1..`[[MaxValue]]. */
  type Error = IllegalArgumentException

  /** Upper bound on a valid signal number - the POSIX `SIGRTMAX` ceiling. */
  inline val MaxValue = 64

  inline def wrap(i: Int): SignalNumber = i
  inline def unwrap(s: SignalNumber): Int = s

  protected inline def validate(i: Int): Option[Error] =
    if i > 0 && i <= MaxValue then None
    else Some(new IllegalArgumentException(s"signal $i out of range (1..$MaxValue)"))

  /** Constructs a [[SignalNumber]], throwing [[Error]] when `value` is out of range. */
  inline def apply(inline value: Int): SignalNumber = fromUnsafe(value)

  val SIGINT: SignalNumber = apply(posix.SIGINT)
  val SIGTERM: SignalNumber = apply(posix.SIGTERM)
  val SIGQUIT: SignalNumber = apply(posix.SIGQUIT)
  val SIGHUP: SignalNumber = apply(posix.SIGHUP)
  val SIGUSR1: SignalNumber = apply(posix.SIGUSR1)
  val SIGUSR2: SignalNumber = apply(posix.SIGUSR2)
  val SIGCHLD: SignalNumber = apply(posix.SIGCHLD)
  val SIGPROF: SignalNumber = apply(posix.SIGPROF)

  /** `SIGKILL` - forced, uncatchable termination. A signal watcher cannot observe it (the kernel
    * never delivers it to a handler); it serves `Process.kill` for an unconditional stop.
    */
  val SIGKILL: SignalNumber = apply(posix.SIGKILL)

  /** `SIGSTOP` - forced, uncatchable suspension. As with [[SIGKILL]] a watcher cannot observe it;
    * resume a stopped process with [[SIGCONT]].
    */
  val SIGSTOP: SignalNumber = apply(posix.SIGSTOP)

  /** `SIGCONT` - resume a process suspended by [[SIGSTOP]]. */
  val SIGCONT: SignalNumber = apply(posix.SIGCONT)

  // scala-native's posix layer does not bind SIGWINCH (it is not in POSIX.1); 28 is its Linux value.
  val SIGWINCH: SignalNumber = apply(28)

end SignalNumber
