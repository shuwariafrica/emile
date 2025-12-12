/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import zio.*
import io.github.arashi01.emile.{Async, EmileError, Open}

/**
 * ZIO Scope integration for Async handles.
 *
 * Async handles allow any thread to wake up the event loop and invoke
 * a callback on the loop thread. This is the primary mechanism for
 * thread-safe communication with the event loop.
 *
 * Provides scoped resource acquisition and safe async cleanup.
 * Handles are closed asynchronously when the enclosing scope closes.
 */
object AsyncResource:
  /**
   * Create an async handle managed by the current Scope.
   *
   * The callback will be invoked on the event loop thread whenever
   * `send` is called from any thread.
   *
   * @param callback The callback to invoke when the async is signaled
   */
  inline def make(callback: () => Unit): ZIO[EmileLoop & Scope, EmileError, Async[Open]] =
    for
      loop <- EmileLoop.loop
      async <- ZIO.acquireRelease(
        acquire = ZIO.fromEither(Async.init(loop)(callback))
      )(
        release = handle => closeAsync(handle)
      )
    yield async

  // ===========================================================================
  // Internal async helpers
  // ===========================================================================

  /**
   * Close handle asynchronously, awaiting callback.
   *
   * Note: Ignores AlreadyClosed errors since handles may be closed
   * by the loop's walkAndClose during finalization.
   */
  private def closeAsync(handle: Async[Open]): UIO[Unit] =
    ZIO.async[Any, Nothing, Unit] { cb =>
      handle.closeAsync(_ => cb(ZIO.unit))
    }
end AsyncResource
