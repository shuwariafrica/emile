/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import zio.*
import io.github.arashi01.emile.{Duration, EmileError, Open, Timer}

/**
 * ZIO Scope integration for Timer handles.
 *
 * Provides scoped resource acquisition and safe async cleanup for timers.
 * Handles are closed asynchronously when the enclosing scope closes.
 */
object TimerResource:
  /**
   * Create a timer handle managed by the current Scope.
   *
   * The handle is closed asynchronously when the scope closes.
   */
  inline def make: ZIO[EmileLoop & Scope, EmileError, Timer[Open]] =
    for
      loop <- EmileLoop.loop
      timer <- ZIO.acquireRelease(
        acquire = ZIO.fromEither(Timer.init(loop))
      )(
        release = handle => closeAsync(handle)
      )
    yield timer

  /**
   * Create a one-shot timer as a scoped resource.
   *
   * The timer fires once after the specified timeout.
   *
   * @param timeout Time until the callback fires
   * @param callback The callback to invoke
   */
  inline def after(timeout: Duration)(callback: () => Unit): ZIO[EmileLoop & Scope, EmileError, Timer[Open]] =
    for
      loop <- EmileLoop.loop
      timer <- ZIO.acquireRelease(
        acquire = ZIO.fromEither(Timer.after(loop, timeout)(callback))
      )(
        release = handle => closeAsync(handle)
      )
    yield timer

  /**
   * Create a repeating timer as a scoped resource.
   *
   * The timer fires repeatedly at the specified interval.
   *
   * @param interval The repeat interval (also used for initial timeout)
   * @param callback The callback to invoke on each tick
   */
  inline def interval(interval: Duration)(callback: () => Unit): ZIO[EmileLoop & Scope, EmileError, Timer[Open]] =
    for
      loop <- EmileLoop.loop
      timer <- ZIO.acquireRelease(
        acquire = ZIO.fromEither(Timer.interval(loop, interval)(callback))
      )(
        release = handle => closeAsync(handle)
      )
    yield timer

  // ===========================================================================
  // Internal async helpers
  // ===========================================================================

  /**
   * Close handle asynchronously, awaiting callback.
   *
   * Note: Ignores AlreadyClosed errors since handles may be closed
   * by the loop's walkAndClose during finalization.
   */
  private def closeAsync(handle: Timer[Open]): UIO[Unit] =
    ZIO.async[Any, Nothing, Unit] { cb =>
      handle.closeAsync(_ => cb(ZIO.unit))
    }
end TimerResource
