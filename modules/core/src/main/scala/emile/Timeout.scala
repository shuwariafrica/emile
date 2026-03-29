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

import scala.annotation.targetName

import boilerplate.*

/** Timeout duration in milliseconds for timer operations.
  *
  * Zero-cost wrapper around Long for type safety. Instances may be constructed via
  * [[Timeout$ Timeout]].
  */
opaque type Timeout = Long

/** Provides factories and extension syntax for [[Timeout]]. */
object Timeout extends OpaqueType[Timeout, Long], OpaqueType.Eq[Timeout]:
  type Error = Nothing

  given Ordering[Timeout] = Ordering.Long

  inline def wrap(value: Long): Timeout = value
  inline def unwrap(value: Timeout): Long = value
  protected inline def validate(value: Long): Option[Nothing] = None
  inline def apply(inline value: Long): Timeout = value

  /** Construct timeout from milliseconds. */
  inline def millis(ms: Long): Timeout = ms

  /** Construct timeout from seconds. */
  inline def seconds(s: Long): Timeout = s * 1000L

  /** Construct timeout from minutes. */
  inline def minutes(m: Long): Timeout = m * 60000L

  /** Construct timeout from hours. */
  inline def hours(h: Long): Timeout = h * 3600000L

  /** Zero timeout constant. */
  val Zero: Timeout = 0L

  extension (t: Timeout)
    /** Get the timeout in milliseconds. */
    inline def toMillis: Long = t

    /** Get the timeout in seconds (truncated). */
    inline def toSeconds: Long = t / 1000L

    @targetName("plus")
    inline def +(other: Timeout): Timeout = t + other

    @targetName("minus")
    inline def -(other: Timeout): Timeout = t - other

    @targetName("times")
    inline def *(factor: Long): Timeout = t * factor

    @targetName("div")
    inline def /(divisor: Long): Timeout = t / divisor

    inline def isZero: Boolean = t == 0L
    inline def isPositive: Boolean = t > 0L
    inline def isNegative: Boolean = t < 0L
  end extension
end Timeout
