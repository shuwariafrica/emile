/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import zio.*
import scala.annotation.internal.sharable
import io.github.arashi01.emile.{EmileError, Loop, LoopConfig, RunMode}

/**
 * Service trait for accessing the libuv event loop.
 *
 * Unlike cats-effect where LibuvPollingSystem makes libuv THE runtime poller,
 * ZIO requires explicit loop management. The loop must be driven by user code
 * or a background fiber.
 *
 * == Usage ==
 *
 * {{{
 * import io.github.arashi01.emile.zio.*
 *
 * val program: ZIO[EmileLoop & Scope, EmileError, Unit] =
 *   for
 *     tcp <- TcpResource.make
 *     _   <- tcp.bind(address).toZIO
 *   yield ()
 *
 * program.provideSome[Scope](EmileLoop.scoped)
 * }}}
 */
trait EmileLoop:
  /** The underlying libuv event loop. */
  def loop: Loop

  /** Run the loop once, blocking until I/O is available. */
  def runOnce: IO[EmileError, Boolean]

  /** Run the loop in non-blocking mode. */
  def runNoWait: IO[EmileError, Boolean]

  /** Run the loop until all handles are closed. */
  def runUntilComplete: IO[EmileError, Unit]

  /** Stop the event loop. */
  def stop: UIO[Unit]

object EmileLoop:
  // ===========================================================================
  // Service accessors (ZIO convention)
  // ===========================================================================

  /** Access the underlying loop. */
  inline def loop: URIO[EmileLoop, Loop] =
    ZIO.serviceWith(_.loop)

  /** Run the loop once, blocking until I/O is available. */
  inline def runOnce: ZIO[EmileLoop, EmileError, Boolean] =
    ZIO.serviceWithZIO(_.runOnce)

  /** Run the loop in non-blocking mode. */
  inline def runNoWait: ZIO[EmileLoop, EmileError, Boolean] =
    ZIO.serviceWithZIO(_.runNoWait)

  /** Run the loop until all handles are closed. */
  inline def runUntilComplete: ZIO[EmileLoop, EmileError, Unit] =
    ZIO.serviceWithZIO(_.runUntilComplete)

  /** Stop the event loop. */
  inline def stop: URIO[EmileLoop, Unit] =
    ZIO.serviceWithZIO(_.stop)

  // ===========================================================================
  // Layer constructors
  // ===========================================================================

  /**
   * Layer that creates a managed loop with default configuration.
   *
   * The loop is properly drained and closed when the scope closes.
   *
   * @note `@sharable` suppresses `-Ycheck-reentrant` warning from ZIO's internal
   *       `ZLayer._hashCode` mutable state which is safely managed by ZIO.
   */
  @sharable
  val scoped: ZLayer[Scope, EmileError, EmileLoop] =
    scoped(LoopConfig.empty)

  /**
   * Layer that creates a managed loop with the specified configuration.
   *
   * @param config Loop configuration options
   * @note `@sharable` suppresses `-Ycheck-reentrant` warning from ZIO's internal
   *       `ZLayer._hashCode` mutable state which is safely managed by ZIO.
   */
  @sharable
  def scoped(config: LoopConfig): ZLayer[Scope, EmileError, EmileLoop] =
    ZLayer.scoped[Any] {
      ZIO.acquireRelease(
        acquire = ZIO.fromEither(Loop.create(config)).map(EmileLoopLive(_))
      )(
        release = live => drainAndClose(live.loop).orDie
      )
    }

  // ===========================================================================
  // Internal implementation
  // ===========================================================================

  /** Live implementation of EmileLoop. */
  private final class EmileLoopLive(val loop: Loop) extends EmileLoop:
    def runOnce: IO[EmileError, Boolean] =
      ZIO.fromEither(loop.run(RunMode.Once))

    def runNoWait: IO[EmileError, Boolean] =
      ZIO.fromEither(loop.run(RunMode.NoWait))

    def runUntilComplete: IO[EmileError, Unit] =
      runOnce.flatMap:
        case true  => runUntilComplete
        case false => ZIO.unit

    def stop: UIO[Unit] =
      ZIO.succeed(loop.stop)
  end EmileLoopLive

  /** Drain a loop (close all handles and process callbacks) then close it. */
  private def drainAndClose(loop: Loop): IO[EmileError, Unit] =
    for
      _ <- ZIO.succeed(loop.walkAndClose())
      _ <- pollUntilDrained(loop)
      _ <- ZIO.fromEither(loop.close)
    yield ()

  /** Poll until the loop has no more active handles. */
  private def pollUntilDrained(loop: Loop): IO[EmileError, Unit] =
    ZIO.suspendSucceed:
      if loop.isAlive then
        val _ = loop.run(RunMode.NoWait)
        ZIO.yieldNow *> pollUntilDrained(loop)
      else
        ZIO.unit
end EmileLoop
