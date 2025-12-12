/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.{IO, Resource}
import io.github.arashi01.emile.{Async, EmileError, Loop, Open}

/**
 * cats-effect Resource integration for Async handles.
 *
 * Async handles allow any thread to wake up the event loop and invoke
 * a callback on the loop thread. This is the primary mechanism for
 * thread-safe communication with the event loop.
 *
 * Requires an integrated loop (via EmileLoop.integrated) for proper async
 * callback processing during resource release.
 */
object AsyncResource:

  /** Helper to lift Either[EmileError, A] to IO[A]. */
  private def liftEmile[A](either: Either[EmileError, A]): IO[A] =
    either.fold(e => IO.raiseError(e), IO.pure)

  /**
   * Create an async handle as a managed resource.
   *
   * The handle is closed asynchronously when the resource is released.
   * The finalizer awaits the close callback before returning.
   *
   * @param callback The callback to invoke when the async is signaled
   * @param loop The event loop (implicit) - should be an integrated loop
   * @return Resource that acquires and safely releases an async handle
   */
  def make(callback: () => Unit)(using loop: Loop): Resource[IO, Async[Open]] =
    Resource.make(
      acquire = liftEmile(Async.init(loop)(callback))
    )(
      release = async => IO.async_ { cb =>
        async.closeAsync(_ => cb(Right(())))
      }
    )
end AsyncResource
