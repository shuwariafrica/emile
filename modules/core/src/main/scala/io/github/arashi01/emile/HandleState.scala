/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

/**
 * Phantom type hierarchy for handle lifecycle state.
 *
 * These types are used as type parameters on handle types to track their
 * state at compile time. This prevents operations on closed handles from
 * even compiling.
 *
 * == Usage ==
 *
 * {{{
 * // Timer.init returns Timer[Open]
 * val timer: Either[EmileError, Timer[Open]] = Timer.init(loop)
 *
 * // Only open timers can be started
 * timer.foreach(_.start(Duration.seconds(1), Duration.Zero)(() => println("Fired!")))
 *
 * // close returns Timer[Closed] - cannot be used for operations
 * val closed: Either[EmileError, Timer[Closed]] = timer.flatMap(_.close)
 * }}}
 *
 * == Design Notes ==
 *
 * The sealed trait hierarchy ensures:
 * - No runtime instances are ever created (phantom types)
 * - Exhaustive type-level matching is possible
 * - Third-party code cannot extend the hierarchy
 */
sealed trait HandleState

/**
 * Phantom type indicating an open, usable handle.
 *
 * Handles in the `Open` state can perform all operations:
 * - Start/stop (Timer)
 * - Send (Async)
 * - Bind/listen/connect (Tcp)
 * - Start/stop polling (Poll)
 */
sealed trait Open extends HandleState

/**
 * Phantom type indicating a closed handle.
 *
 * Handles in the `Closed` state cannot be used for any operations.
 * The only valid operation is checking the type for tracking purposes.
 *
 * Note: libuv close is asynchronous, so `Closed` means "close initiated"
 * not necessarily "close completed". The handle memory is freed when
 * the close callback fires.
 */
sealed trait Closed extends HandleState
