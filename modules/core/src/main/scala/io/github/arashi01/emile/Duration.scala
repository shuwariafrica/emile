/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.annotation.targetName

/**
 * Duration in milliseconds for timer and timeout operations.
 *
 * This is a zero-cost wrapper around Long for type safety.
 */
opaque type Duration = Long

object Duration:
  given CanEqual[Duration, Duration] = CanEqual.derived
  given Ordering[Duration] = Ordering.Long

  /** Construct duration from milliseconds. */
  inline def millis(ms: Long): Duration = ms

  /** Construct duration from seconds. */
  inline def seconds(s: Long): Duration = s * 1000L

  /** Construct duration from minutes. */
  inline def minutes(m: Long): Duration = m * 60000L

  /** Construct duration from hours. */
  inline def hours(h: Long): Duration = h * 3600000L

  /** Zero duration constant. */
  val Zero: Duration = 0L

  extension (d: Duration)
    /** Get the duration in milliseconds. */
    inline def toMillis: Long = d

    /** Get the duration in seconds (truncated). */
    inline def toSeconds: Long = d / 1000L

    /** Add two durations. */
    @targetName("plus")
    inline def +(other: Duration): Duration = d + other

    /** Subtract a duration. */
    @targetName("minus")
    inline def -(other: Duration): Duration = d - other

    /** Multiply by a factor. */
    @targetName("times")
    inline def *(factor: Long): Duration = d * factor

    /** Divide by a divisor. */
    @targetName("div")
    inline def /(divisor: Long): Duration = d / divisor

    /** Check if duration is zero. */
    inline def isZero: Boolean = d == 0L

    /** Check if duration is positive. */
    inline def isPositive: Boolean = d > 0L

    /** Check if duration is negative. */
    inline def isNegative: Boolean = d < 0L
end Duration
