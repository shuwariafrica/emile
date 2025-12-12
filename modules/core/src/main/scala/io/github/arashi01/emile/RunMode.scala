/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

/**
 * Run mode for the event loop.
 *
 * Controls how `Loop.run` processes events.
 */
enum RunMode derives CanEqual:
  /** Run until no more active handles/requests. Default mode. */
  case Default

  /** Poll once, blocking if no pending callbacks. */
  case Once

  /** Poll once, non-blocking. Returns immediately if no pending events. */
  case NoWait
end RunMode

object RunMode:
  /** Convert to libuv UV_RUN_* constant. */
  extension (mode: RunMode)
    private[emile] inline def toLibuv: Int = mode match
      case Default => 0  // UV_RUN_DEFAULT
      case Once    => 1  // UV_RUN_ONCE
      case NoWait  => 2  // UV_RUN_NOWAIT
end RunMode
