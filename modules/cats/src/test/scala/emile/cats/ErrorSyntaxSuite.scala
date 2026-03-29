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

import cats.effect.IO
import cats.syntax.all.*

import boilerplate.effect.Eff
import boilerplate.effect.eff

import emile.EmileError
import emile.cats.syntax.all.*

/** Tests for ErrorSyntax - error handling extension methods and extractors. */
class ErrorSyntaxSuite extends EmileSuite:

  // ============================================================================
  // Either.eff extension
  // ============================================================================

  test("eff[IO] converts Right to successful Eff") {
    val either: Either[EmileError, Int] = Right(42)
    either.eff[IO].rethrow.map { value =>
      assertEquals(value, 42)
    }
  }

  test("eff[IO] converts Left to failed Eff with EmileError") {
    val either: Either[EmileError, Int] = Left(EmileError.AlreadyClosed)
    either.eff[IO].rethrow.attempt.map {
      case Left(e: EmileError) => assertEquals(e, EmileError.AlreadyClosed)
      case other               => fail(s"Expected EmileError.AlreadyClosed, got $other")
    }
  }

  test("eff[IO] preserves error type through chain") {
    val either: Either[EmileError, Int] = Left(EmileError.Cancelled)
    either
      .eff[IO]
      .flatMap { v =>
        boilerplate.effect.Eff.succeed[IO, EmileError, Int](v + 1)
      }
      .rethrow
      .attempt
      .map {
        case Left(e: EmileError) => assertEquals(e, EmileError.Cancelled)
        case other               => fail(s"Expected EmileError.Cancelled, got $other")
      }
  }

  // ============================================================================
  // IO.catchEmile extension
  // ============================================================================

  test("catchEmile recovers from EmileError") {
    val io = IO.raiseError[Int](EmileError.TimedOut)
    io.catchEmile { case EmileError.TimedOut =>
      IO.pure(0)
    }.map { value =>
      assertEquals(value, 0)
    }
  }

  test("catchEmile does not catch non-matching errors") {
    val io = IO.raiseError[Int](EmileError.AlreadyClosed)
    io.catchEmile { case EmileError.TimedOut =>
      IO.pure(0)
    }.attempt
      .map {
        case Left(e: EmileError) => assertEquals(e, EmileError.AlreadyClosed)
        case other               => fail(s"Expected EmileError.AlreadyClosed, got $other")
      }
  }

  test("catchEmile does not catch non-EmileError exceptions") {
    val io = IO.raiseError[Int](new RuntimeException("not emile"))
    io.catchEmile { case EmileError.TimedOut =>
      IO.pure(0)
    }.attempt
      .map {
        case Left(e: RuntimeException) => assertEquals(e.getMessage, "not emile")
        case other                     => fail(s"Expected RuntimeException, got $other")
      }
  }

  test("catchEmile can recover with effectful computation") {
    val io = IO.raiseError[Int](EmileError.TimedOut)
    io.catchEmile { case EmileError.TimedOut =>
      IO.delay(42)
    }.map { value =>
      assertEquals(value, 42)
    }
  }

  // ============================================================================
  // Emile extractor
  // ============================================================================

  test("Emile extractor works in pattern matching") {
    val error: Throwable = EmileError.Cancelled
    error match
      case Emile(EmileError.Cancelled) => () // Success
      case _                           => fail("Pattern should match")
  }

  test("Emile extractor does not match non-EmileError") {
    val error: Throwable = new RuntimeException("not emile")
    error match
      case Emile(_) => fail("Should not match RuntimeException")
      case _        => () // Success
  }

  test("Emile extractor enables typed error handling in attempt") {
    IO.raiseError[Int](EmileError.AlreadyClosed).attempt.map {
      case Left(Emile(EmileError.AlreadyClosed)) => () // Success
      case Left(Emile(other))                    => fail(s"Wrong EmileError: $other")
      case Left(other)                           => fail(s"Non-EmileError: $other")
      case Right(_)                              => fail("Expected error")
    }
  }

  // ============================================================================
  // toValidatedNec extensions
  // ============================================================================

  test("toValidatedNec accumulates error list") {
    val validated = List[EmileError](EmileError.AlreadyClosed, EmileError.TimedOut).toValidatedNec
    validated.fold(
      nec => assertEquals(nec.toList.size, 2),
      _ => fail("Expected validation failure")
    )
  }

  test("toValidatedNec on empty list returns Valid") {
    val validated = List.empty[EmileError].toValidatedNec
    validated.fold(
      _ => fail("Expected valid for empty error list"),
      _ => () // Success - empty error list is valid
    )
  }

  test("Either[List[EmileError], A].toValidatedNec converts Left to NonEmptyChain") {
    val validated = Left(List(EmileError.Cancelled)).toValidatedNec
    validated.fold(
      nec => assertEquals(nec.head, EmileError.Cancelled),
      _ => fail("Expected validation failure")
    )
  }

  test("Either[List[EmileError], A].toValidatedNec converts Right to Valid") {
    val validated: cats.data.ValidatedNec[EmileError, Int] = Right(42).toValidatedNec
    validated.fold(
      _ => fail("Expected valid"),
      v => assertEquals(v, 42)
    )
  }

end ErrorSyntaxSuite
