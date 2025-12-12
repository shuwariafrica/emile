/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.{IO, Resource}
import io.github.arashi01.emile.{Duration, EmileError, Loop, Open, Timer}

/**
 * cats-effect Resource integration for Timer handles.
 *
 * Provides managed resource acquisition and safe async cleanup for timers.
 * Requires an integrated loop (via EmileLoop.integrated) for proper async
 * callback processing during resource release.
 */
object TimerResource:

  /** Helper to lift Either[EmileError, A] to IO[A]. */
  private def liftEmile[A](either: Either[EmileError, A]): IO[A] =
    either.fold(e => IO.raiseError(e), IO.pure)

  /**
   * Create a timer handle as a managed resource.
   *
   * The handle is closed asynchronously when the resource is released.
   * The finalizer awaits the close callback before returning.
   *
   * @param loop The event loop (implicit) - should be an integrated loop
   * @return Resource that acquires and safely releases a timer
   */
  def make(using loop: Loop): Resource[IO, Timer[Open]] =
    Resource.make(
      acquire = liftEmile(Timer.init(loop))
    )(
      release = timer => IO.async_ { cb =>
        timer.closeAsync(_ => cb(Right(())))
      }
    )

  /**
   * Create a one-shot timer as a resource.
   *
   * The timer fires once after the specified timeout.
   *
   * @param timeout Time until the callback fires
   * @param callback The callback to invoke
   * @param loop The event loop (implicit)
   * @return Resource that acquires a started timer
   */
  def after(timeout: Duration)(callback: () => Unit)(using loop: Loop): Resource[IO, Timer[Open]] =
    Resource.make(
      acquire = liftEmile(Timer.after(loop, timeout)(callback))
    )(
      release = timer => IO.async_ { cb =>
        timer.closeAsync(_ => cb(Right(())))
      }
    )

  /**
   * Create a repeating timer as a resource.
   *
   * The timer fires repeatedly at the specified interval.
   *
   * @param interval The repeat interval (also used for initial timeout)
   * @param callback The callback to invoke on each tick
   * @param loop The event loop (implicit)
   * @return Resource that acquires a started repeating timer
   */
  def interval(interval: Duration)(callback: () => Unit)(using loop: Loop): Resource[IO, Timer[Open]] =
    Resource.make(
      acquire = liftEmile(Timer.interval(loop, interval)(callback))
    )(
      release = timer => IO.async_ { cb =>
        timer.closeAsync(_ => cb(Right(())))
      }
    )
end TimerResource
