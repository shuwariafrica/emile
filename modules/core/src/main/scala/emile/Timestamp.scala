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

/** Monotonic timestamp in milliseconds from the event loop.
  *
  * Obtained from `Loop.now` which returns a cached timestamp. Instances may be constructed via
  * [[Timestamp$ Timestamp]].
  */
opaque type Timestamp = Long

/** Provides factories and extension syntax for [[Timestamp]]. */
object Timestamp extends OpaqueType[Timestamp, Long], OpaqueType.Eq[Timestamp]:
  type Error = Nothing

  given Ordering[Timestamp] = Ordering.Long

  inline def wrap(value: Long): Timestamp = value
  inline def unwrap(value: Timestamp): Long = value
  protected inline def validate(value: Long): Option[Nothing] = None
  inline def apply(inline value: Long): Timestamp = value

  extension (t: Timestamp)
    /** Get the raw milliseconds value. */
    inline def millis: Long = t

    @targetName("minus")
    inline def -(other: Timestamp): Timeout = Timeout.millis(t - other)

    @targetName("plus")
    inline def +(duration: Timeout): Timestamp = t + duration.toMillis

    inline def isBefore(other: Timestamp): Boolean = t < other
    inline def isAfter(other: Timestamp): Boolean = t > other
  end extension
end Timestamp
