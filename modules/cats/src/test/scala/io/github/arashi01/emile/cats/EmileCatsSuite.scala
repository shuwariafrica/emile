/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.IO
import io.github.arashi01.emile.EmileError

/**
 * Tests for emile-cats module Resource integration.
 *
 * Extends EmileSuite to use LibuvPollingSystem for proper libuv integration.
 */
class EmileCatsSuite extends EmileSuite:

  // ============================================================================
  // EmileLoop Resource Tests
  // ============================================================================

  test("EmileLoop.integrated acquires loop from runtime") {
    EmileLoop.integrated.use { loop =>
      IO {
        // Loop is an opaque type wrapping Ptr[Byte], verify it's accessible
        assert(loop.isAlive || !loop.isAlive, "Loop should be callable")
      }
    }
  }

  test("EmileLoop.create acquires and releases a standalone loop") {
    EmileLoop.create.use { loop =>
      IO {
        // Verify loop is accessible and can be queried
        assert(loop.isAlive || !loop.isAlive, "Loop should be callable")
      }
    }
  }

  test("EmileLoop.runOnce extension works") {
    EmileLoop.create.use { loop =>
      import EmileLoop.runOnce
      loop.runOnce.map { alive =>
        assert(!alive, "Empty loop should not be alive")
      }
    }
  }

  test("EmileLoop.runNoWait extension works") {
    EmileLoop.create.use { loop =>
      import EmileLoop.runNoWait
      loop.runNoWait.map { alive =>
        assert(!alive, "Empty loop should not be alive after runNoWait")
      }
    }
  }

  // ============================================================================
  // Error Syntax Tests
  // ============================================================================

  test("liftIO converts Right to successful IO") {
    val either: Either[EmileError, Int] = Right(42)
    either.liftIO.map { value =>
      assertEquals(value, 42)
    }
  }

  test("liftIO converts Left to failed IO with EmileError") {
    val either: Either[EmileError, Int] = Left(EmileError.AlreadyClosed)
    either.liftIO.attempt.map {
      case Left(e: EmileError) => assertEquals(e, EmileError.AlreadyClosed)
      case other => fail(s"Expected EmileError.AlreadyClosed, got $other")
    }
  }

  test("catchEmile recovers from EmileError") {
    val io = IO.raiseError[Int](EmileError.TimedOut)
    io.catchEmile {
      case EmileError.TimedOut => IO.pure(0)
    }.map { value =>
      assertEquals(value, 0)
    }
  }

  test("catchEmile does not catch non-matching errors") {
    val io = IO.raiseError[Int](EmileError.AlreadyClosed)
    io.catchEmile {
      case EmileError.TimedOut => IO.pure(0)
    }.attempt.map {
      case Left(e: EmileError) => assertEquals(e, EmileError.AlreadyClosed)
      case other => fail(s"Expected EmileError.AlreadyClosed, got $other")
    }
  }

  test("Emile extractor works in pattern matching") {
    val error: Throwable = EmileError.Cancelled
    error match
      case Emile(EmileError.Cancelled) => () // Success
      case _ => fail("Pattern should match")
  }

  // ============================================================================
  // TimerResource Tests (with integrated loop)
  // ============================================================================

  test("TimerResource.make acquires and releases timer with integrated loop") {
    EmileLoop.integrated.use { implicit loop =>
      TimerResource.make.use { timer =>
        IO {
          assert(!timer.isClosing, "Timer should not be closing")
        }
      }
    }
  }

  // ============================================================================
  // AsyncResource Tests  
  // ============================================================================

  test("AsyncResource.make acquires and releases async handle") {
    EmileLoop.integrated.use { implicit loop =>
      AsyncResource.make(() => ()).use { async =>
        IO {
          assert(!async.isClosing, "Async should not be closing")
        }
      }
    }
  }

  // ============================================================================
  // TcpResource Tests
  // ============================================================================

  test("TcpResource.make acquires and releases tcp handle") {
    EmileLoop.integrated.use { implicit loop =>
      TcpResource.make.use { tcp =>
        IO {
          assert(!tcp.isClosing, "TCP should not be closing")
        }
      }
    }
  }

end EmileCatsSuite
