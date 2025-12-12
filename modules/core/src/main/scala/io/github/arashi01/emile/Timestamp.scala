/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.annotation.targetName

/**
 * Monotonic timestamp in milliseconds from the event loop.
 *
 * Obtained from `Loop.now` which returns a cached timestamp.
 * Use `Loop.updateTime` to refresh the cached value.
 */
opaque type Timestamp = Long

object Timestamp:
  given CanEqual[Timestamp, Timestamp] = CanEqual.derived
  given Ordering[Timestamp] = Ordering.Long

  /** Construct from milliseconds since arbitrary epoch. */
  inline def apply(millis: Long): Timestamp = millis

  extension (t: Timestamp)
    /** Get the raw milliseconds value. */
    inline def millis: Long = t

    /** Calculate duration between two timestamps. */
    @targetName("minus")
    inline def -(other: Timestamp): Duration = Duration.millis(t - other)

    /** Add a duration to get a future timestamp. */
    @targetName("plus")
    inline def +(duration: Duration): Timestamp = t + duration.toMillis

    /** Check if this timestamp is before another. */
    inline def isBefore(other: Timestamp): Boolean = t < other

    /** Check if this timestamp is after another. */
    inline def isAfter(other: Timestamp): Boolean = t > other
end Timestamp
