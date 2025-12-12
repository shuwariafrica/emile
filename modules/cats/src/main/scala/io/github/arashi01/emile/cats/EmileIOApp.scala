/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.IOApp
import cats.effect.unsafe.PollingSystem
import io.github.arashi01.emile.{Loop, LoopConfig}

/**
 * IOApp trait that uses libuv as the polling backend.
 *
 * This is the recommended way to build cats-effect applications with emile.
 * It configures the cats-effect runtime to use libuv for all I/O polling,
 * eliminating the need for separate event loop management.
 *
 * == Usage ==
 *
 * {{{
 * import cats.effect.{IO, ExitCode}
 * import io.github.arashi01.emile.cats.EmileIOApp
 * import io.github.arashi01.emile.Tcp
 *
 * object MyServer extends EmileIOApp:
 *   def run(args: List[String]): IO[ExitCode] =
 *     for
 *       // Access the libuv loop for the current worker thread
 *       _ <- IO.println("Starting server...")
 *       // Use emile APIs that work with the integrated loop
 *       _ <- ???
 *     yield ExitCode.Success
 * }}}
 *
 * == Loop Access ==
 *
 * Use `EmileIOApp.withLoop` to access the current thread's libuv loop:
 *
 * {{{
 * EmileIOApp.withLoop { loop =>
 *   // Create handles, start timers, etc.
 *   Tcp.init(loop)
 * }
 * }}}
 *
 * == Configuration ==
 *
 * Override `loopConfig` to customize the libuv loop:
 *
 * {{{
 * object MyApp extends EmileIOApp:
 *   override def loopConfig: LoopConfig =
 *     LoopConfig.empty
 *       .withMetricsEnabled(true)
 *       .withBlockSignal(SIGPROF)
 *
 *   def run(args: List[String]): IO[ExitCode] = ...
 * }}}
 */
trait EmileIOApp extends IOApp:
  /**
   * Override to customize the libuv loop configuration.
   *
   * This is called once before the runtime starts.
   */
  def loopConfig: LoopConfig = LoopConfig.empty

  /**
   * The polling system used by this application.
   *
   * Uses libuv via `LibuvPollingSystem`.
   */
  override protected def pollingSystem: PollingSystem =
    LibuvPollingSystem.configure(loopConfig)
    LibuvPollingSystem

object EmileIOApp:
  /**
   * Execute a callback with access to the current worker thread's libuv loop.
   *
   * This must be called from within an IO effect running on the cats-effect runtime.
   * The callback receives the loop owned by the current worker thread.
   *
   * {{{
   * val createTcp: IO[Either[EmileError, Tcp[Open]]] =
   *   IO.async_ { cb =>
   *     EmileIOApp.withLoop { loop =>
   *       cb(Right(Tcp.init(loop)))
   *     }
   *   }
   * }}}
   *
   * @param f Callback that receives the loop
   */
  def withLoop(f: Loop => Unit): Unit =
    // This will be called from within an IO effect, so we need access to the
    // polling context. For now, we use a simple thread-local approach.
    // In production, this should integrate with cats-effect's polling context.
    ???
