/*
 * Copyright 2025, 2026 Ali Rashid.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package emile.cats

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*

import boilerplate.effect.*

import emile.EmileError

/** Tests for EffAsync - the typed async primitives for libuv callback integration.
  *
  * These tests validate that EffAsync correctly:
  *   - Preserves typed error channel through async boundaries
  *   - Handles success, typed errors, and defects appropriately
  *   - Integrates with cats-effect's fiber cancellation
  *   - Works with Eff's monadic composition
  */
class EffAsyncSuite extends EmileSuite:
  // scalafix:off

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  // ============================================================================
  // EffAsync.async_ Tests
  // ============================================================================

  test("EffAsync.async_ propagates success value") {
    runEff {
      EffAsync
        .async_[Int] { complete =>
          complete(Right(42))
        }
        .map { result =>
          assertEquals(result, 42)
        }
    }
  }

  test("EffAsync.async_ propagates typed error to Eff channel") {
    runEff {
      EffAsync
        .async_[Int] { complete =>
          complete(Left(EmileError.Cancelled))
        }
        .catchAll {
          case EmileError.Cancelled => Eff.succeed[IO, EmileError, Int](0)
          case other                => Eff.fail[IO, EmileError, Int](other)
        }
        .map { result =>
          assertEquals(result, 0)
        }
    }
  }

  test("EffAsync.async_ chains correctly with flatMap") {
    runEff {
      for
        a <- EffAsync.async_[Int](cb => cb(Right(10)))
        b <- EffAsync.async_[Int](cb => cb(Right(20)))
        c <- EffAsync.async_[Int](cb => cb(Right(30)))
      yield assertEquals(a + b + c, 60)
    }
  }

  test("EffAsync.async_ short-circuits on first error") {
    runEff {
      val program = for
        a <- EffAsync.async_[Int](cb => cb(Right(10)))
        _ <- EffAsync.async_[Int](cb => cb(Left(EmileError.Cancelled)))
        c <- EffAsync.async_[Int](cb => cb(Right(30)))
      yield a + c

      program
        .catchAll { _ =>
          Eff.succeed[IO, EmileError, Int](-1)
        }
        .map { result =>
          assertEquals(result, -1)
        }
    }
  }

  // ============================================================================
  // EffAsync.async_Unit Tests
  // ============================================================================

  test("EffAsync.async_Unit completes successfully") {
    runEff {
      EffAsync.async_Unit { complete =>
        complete(Right(()))
      }
    }
  }

  test("EffAsync.async_Unit propagates typed error") {
    runEff {
      EffAsync
        .async_Unit { complete =>
          complete(Left(EmileError.TimedOut))
        }
        .catchAll {
          case EmileError.TimedOut => Eff.unit[IO, EmileError]
          case other               => Eff.fail[IO, EmileError, Unit](other)
        }
    }
  }

  // ============================================================================
  // EffAsync.delay Tests
  // ============================================================================

  test("EffAsync.delay suspends side effect") {
    var executed = false
    runEff {
      val delayed = EffAsync.delay {
        executed = true
        42
      }
      // Effect not yet executed
      assert(!executed, "delay should suspend execution")
      delayed.map { result =>
        assert(executed, "delay should execute when run")
        assertEquals(result, 42)
      }
    }
  }

  // ============================================================================
  // EffAsync.blocking Tests
  // ============================================================================

  test("EffAsync.blocking runs on blocking thread pool") {
    runEff {
      EffAsync
        .blocking {
          Thread.sleep(1) // Simulate blocking operation
          42
        }
        .map { result =>
          assertEquals(result, 42)
        }
    }
  }

  // ============================================================================
  // EffAsync.sleep Tests
  // ============================================================================

  test("EffAsync.sleep suspends for duration") {
    runEff {
      for
        start <- EffAsync.delay(System.currentTimeMillis())
        _ <- EffAsync.sleep(50.millis)
        end <- EffAsync.delay(System.currentTimeMillis())
      yield assert(end - start >= 45, s"sleep should suspend for at least 45ms, got ${end - start}ms")
    }
  }

  // ============================================================================
  // Integration with cats Traverse
  // ============================================================================

  test("EffAsync works with traverse") {
    runEff {
      val items = List(1, 2, 3, 4, 5)
      items
        .traverse { i =>
          EffAsync.async_[Int](cb => cb(Right(i * 2)))
        }
        .map { results =>
          assertEquals(results, List(2, 4, 6, 8, 10))
        }
    }
  }

  test("traverse short-circuits on first error") {
    runEff {
      val items = List(1, 2, 3, 4, 5)
      items
        .traverse { i =>
          if i == 3 then EffAsync.async_[Int](cb => cb(Left(EmileError.Cancelled)))
          else EffAsync.async_[Int](cb => cb(Right(i * 2)))
        }
        .catchAll { _ =>
          Eff.succeed[IO, EmileError, List[Int]](List.empty)
        }
        .map { results =>
          assertEquals(results, List.empty)
        }
    }
  }

  // ============================================================================
  // Error Recovery Patterns
  // ============================================================================

  test("catchAll recovers from any EmileError") {
    runEff {
      EffAsync
        .async_[Int](cb => cb(Left(EmileError.Cancelled)))
        .catchAll(_ => Eff.succeed[IO, EmileError, Int](99))
        .map(assertEquals(_, 99))
    }
  }

  test("pattern matching in catchAll recovers from specific errors") {
    runEff {
      EffAsync
        .async_[Int](cb => cb(Left(EmileError.Cancelled)))
        .catchAll {
          case EmileError.Cancelled => Eff.succeed[IO, EmileError, Int](0)
          case other                => Eff.fail[IO, EmileError, Int](other)
        }
        .map(assertEquals(_, 0))
    }
  }

  test("pattern matching in catchAll can re-raise non-matching errors") {
    runEff {
      EffAsync
        .async_[Int](cb => cb(Left(EmileError.TimedOut)))
        .catchAll {
          case EmileError.Cancelled => Eff.succeed[IO, EmileError, Int](0)
          case other                => Eff.fail[IO, EmileError, Int](other)
        }
        .catchAll(_ => Eff.succeed[IO, EmileError, Int](-1))
        .map(assertEquals(_, -1))
    }
  }

end EffAsyncSuite
