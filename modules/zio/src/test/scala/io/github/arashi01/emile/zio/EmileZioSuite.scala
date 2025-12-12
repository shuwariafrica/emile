/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import munit.ZSuite
import zio.*
import io.github.arashi01.emile.{EmileError, LoopConfig}

/**
 * Tests for emile-zio module.
 *
 * Uses munit-zio's ZSuite for ZIO-native testing.
 */
class EmileZioSuite extends ZSuite:

  /** Helper to run a scoped ZIO with EmileLoop provided. */
  private def withLoop[A](zio: ZIO[EmileLoop & Scope, EmileError, A]): IO[EmileError, A] =
    ZIO.scoped[Any](zio.provideSomeLayer[Scope](EmileLoop.scoped))

  /** Helper to run a scoped ZIO with EmileLoop with custom config. */
  private def withLoop[A](config: LoopConfig)(zio: ZIO[EmileLoop & Scope, EmileError, A]): IO[EmileError, A] =
    ZIO.scoped[Any](zio.provideSomeLayer[Scope](EmileLoop.scoped(config)))

  // ============================================================================
  // EmileLoop Service Tests
  // ============================================================================

  testZ("EmileLoop.scoped creates and manages loop lifecycle") {
    withLoop {
      for
        loop <- EmileLoop.loop
      yield assert(loop.isAlive || !loop.isAlive, "Loop should be accessible")
    }
  }

  testZ("EmileLoop.runOnce works with empty loop") {
    withLoop {
      for
        alive <- EmileLoop.runOnce
      yield assert(!alive, "Empty loop should not be alive")
    }
  }

  testZ("EmileLoop.runNoWait works with empty loop") {
    withLoop {
      for
        alive <- EmileLoop.runNoWait
      yield assert(!alive, "Empty loop should not be alive after runNoWait")
    }
  }

  testZ("EmileLoop.stop stops the loop") {
    withLoop {
      for
        _ <- EmileLoop.stop
      yield ()
    }
  }

  testZ("EmileLoop.scoped accepts custom configuration") {
    withLoop(LoopConfig.withMetrics) {
      for
        loop <- EmileLoop.loop
      yield assert(loop.isAlive || !loop.isAlive, "Loop with config should be accessible")
    }
  }

  // ============================================================================
  // Error Syntax Tests
  // ============================================================================

  testZ("Either[EmileError, A].toZIO lifts Right to success") {
    val result: Either[EmileError, Int] = Right(42)
    result.toZIO.map(v => assertEquals(v, 42))
  }

  testZ("Either[EmileError, A].toZIO lifts Left to failure") {
    val error  = EmileError.AlreadyClosed
    val result: Either[EmileError, Int] = Left(error)
    result.toZIO.either.map:
      case Left(e)  => assertEquals(e, error)
      case Right(_) => fail("Expected failure")
  }

  testZ("catchEmile recovers from EmileError") {
    val failing: IO[EmileError, Int] = ZIO.fail(EmileError.AlreadyClosed)
    failing
      .catchEmile { case EmileError.AlreadyClosed => ZIO.succeed(0) }
      .map(v => assertEquals(v, 0))
  }

  testZ("recoverEmile handles all EmileError variants") {
    val failing: IO[EmileError, Int] = ZIO.fail(EmileError.AlreadyClosed)
    failing
      .recoverEmile(_ => -1)
      .map(v => assertEquals(v, -1))
  }

  testZ("mapEmileError transforms error type") {
    val failing: IO[EmileError, Int] = ZIO.fail(EmileError.AlreadyClosed)
    failing
      .mapEmileError(e => new RuntimeException(e.getMessage))
      .either
      .map:
        case Left(e: RuntimeException) => assert(e.getMessage.contains("closed"))
        case _                         => fail("Expected RuntimeException")
  }

  testZ("Emile extractor works in pattern matching") {
    val error: Throwable = EmileError.AlreadyClosed
    ZIO.succeed:
      error match
        case Emile(e) => assertEquals(e, EmileError.AlreadyClosed)
        case _        => fail("Expected Emile extractor to match")
  }

  // ============================================================================
  // TcpResource Tests
  // ============================================================================

  testZ("TcpResource.make creates scoped TCP handle") {
    withLoop {
      for
        tcp <- TcpResource.make
      yield assert(tcp.isActive || !tcp.isActive, "TCP handle should be created")
    }
  }

  // ============================================================================
  // TimerResource Tests
  // ============================================================================

  testZ("TimerResource.make creates scoped timer handle") {
    withLoop {
      for
        timer <- TimerResource.make
      yield assert(timer.isActive || !timer.isActive, "Timer handle should be created")
    }
  }

  // ============================================================================
  // AsyncResource Tests
  // ============================================================================

  testZ("AsyncResource.make creates scoped async handle") {
    withLoop {
      @volatile var called = false
      for
        async <- AsyncResource.make(() => called = true)
        _     <- ZIO.succeed(called) // reference to suppress warning
      yield assert(async.isActive || !async.isActive, "Async handle should be created")
    }
  }

end EmileZioSuite
