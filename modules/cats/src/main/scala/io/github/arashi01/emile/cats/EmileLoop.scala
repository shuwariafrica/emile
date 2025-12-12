/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.{IO, Resource}
import io.github.arashi01.emile.{EmileError, Loop, LoopConfig, RunMode}
import io.github.arashi01.emile.cats.LibuvPollingSystem.LoopAccess

/**
 * cats-effect Resource integration for libuv event loop.
 *
 * Provides managed resource acquisition for libuv loops in a cats-effect context.
 * Requires `LibuvPollingSystem` as the runtime's polling backend.
 *
 * == Usage ==
 *
 * {{{
 * // In an EmileIOApp or runtime with LibuvPollingSystem:
 * EmileLoop.integrated.use { loop =>
 *   // Create handles, perform I/O, etc.
 *   IO.println(s"Loop alive: \${loop.isAlive}")
 * }
 * }}}
 *
 * == Thread Model ==
 *
 * Each cats-effect worker thread has its own libuv loop. `integrated` provides
 * access to the current thread's loop. Handles created on a loop belong to that
 * loop and must be used from the same thread.
 */
object EmileLoop:

  /** Helper to lift Either[EmileError, A] to IO[A]. */
  private def liftEmile[A](either: Either[EmileError, A]): IO[A] =
    either.fold(e => IO.raiseError(e), IO.pure)

  /**
   * Get the current thread's libuv loop from the runtime.
   *
   * This is the primary way to access a loop in emile-cats. The loop is
   * owned by the runtime and is already being polled - no additional
   * setup is needed.
   *
   * The loop is NOT closed when the resource is released (it belongs
   * to the runtime).
   *
   * @return Resource providing access to the current thread's loop
   */
  val integrated: Resource[IO, Loop] =
    LoopAccess.get.toResource.flatMap(_.loop)



  /**
   * Create a standalone loop for manual management.
   *
   * Use this when you need a loop separate from the runtime's loops,
   * for example for dedicated I/O threads or testing. You are responsible
   * for running the loop (via `runOnce`, `runNoWait`, etc.).
   *
   * The loop is properly drained and closed when the resource is released.
   */
  def create: Resource[IO, Loop] =
    create(LoopConfig.empty)

  /**
   * Create a standalone loop with configuration.
   *
   * @param config Loop configuration options
   * @return Resource that manages loop lifecycle
   */
  def create(config: LoopConfig): Resource[IO, Loop] =
    Resource.make(
      acquire = liftEmile(Loop.create(config))
    )(
      release = loop => drainAndClose(loop)
    )

  /**
   * Drain a loop (close all handles and process callbacks) then close it.
   */
  private def drainAndClose(loop: Loop): IO[Unit] =
    for
      _ <- IO(loop.walkAndClose())
      _ <- pollUntilDrained(loop)
      _ <- IO(loop.close).void
    yield ()

  /**
   * Poll until the loop has no more active handles.
   */
  private def pollUntilDrained(loop: Loop): IO[Unit] =
    IO {
      if loop.isAlive then
        val _ = loop.run(RunMode.NoWait)
        true
      else
        false
    }.flatMap { alive =>
      if alive then IO.cede *> pollUntilDrained(loop)
      else IO.unit
    }

  // =========================================================================
  // Loop extensions for IO
  // =========================================================================

  extension (loop: Loop)
    /**
     * Run loop once, blocking until I/O is available.
     *
     * @return true if loop still has active handles
     */
    def runOnce: IO[Boolean] =
      liftEmile(loop.run(RunMode.Once))

    /**
     * Run loop until all handles are closed.
     *
     * Blocks the fiber until no active handles remain.
     */
    def runUntilComplete: IO[Unit] =
      loop.runOnce.flatMap { alive =>
        if alive then loop.runUntilComplete else IO.unit
      }

    /**
     * Run loop in non-blocking mode.
     *
     * Processes pending callbacks without waiting for I/O.
     *
     * @return true if loop still has active handles
     */
    def runNoWait: IO[Boolean] =
      liftEmile(loop.run(RunMode.NoWait))

end EmileLoop

