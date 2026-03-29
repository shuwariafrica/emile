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

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec
import cats.effect.IO

import boilerplate.effect.*
import boilerplate.effect.Eff

import emile.EmileError

/** Error handling syntax for Eff-based typed error channels.
  *
  * Provides conversions between Eff[IO, EmileError, A] and IO[A], plus utilities for working with
  * EmileError in IO contexts.
  */

extension [A](eff: Eff[IO, EmileError, A])
  /** Raise EmileError into IO's Throwable channel.
    *
    * Converts Eff[IO, EmileError, A] → IO[A], raising EmileError as throwables. Since EmileError
    * extends Throwable, this is zero-cost.
    */
  inline def rethrow: IO[A] = eff.either.flatMap {
    case Right(a) => IO.pure(a)
    case Left(e)  => IO.raiseError(e)
  }

extension [A](io: IO[A])
  /** Recover from EmileError using partial function.
    *
    * @param pf Partial function from EmileError to recovery action
    */
  inline def catchEmile(pf: PartialFunction[EmileError, IO[A]]): IO[A] =
    io.handleErrorWith {
      case e: EmileError if pf.isDefinedAt(e) => pf(e)
      case other                              => IO.raiseError(other)
    }

  /** Handle specific EmileError variants.
    *
    * @param f Function from EmileError to recovery value
    */
  inline def recoverEmile(f: EmileError => A): IO[A] =
    io.handleErrorWith {
      case e: EmileError => IO.pure(f(e))
      case other         => IO.raiseError(other)
    }
end extension

/** Pattern extractor for EmileError in error handlers.
  *
  * Usage:
  * {{{
  * io.handleErrorWith {
  *   case Emile(EmileError.SystemError(code, _)) => fallback
  *   case other => IO.raiseError(other)
  * }
  * }}}
  */
object Emile:
  inline def unapply(t: Throwable): Option[EmileError] = t match
    case e: EmileError => Some(e)
    case _             => None

// ============================================================================
// Validation helpers
// ============================================================================

extension (errors: List[EmileError])
  /** Convert a list of errors into a ValidatedNec accumulator. */
  inline def toValidatedNec: ValidatedNec[EmileError, Unit] =
    NonEmptyChain.fromSeq(errors) match
      case Some(nec) => Validated.invalid(nec)
      case None      => Validated.valid(())

extension [A](either: Either[List[EmileError], A])
  /** Convert Either[List[EmileError], A] into ValidatedNec[EmileError, A]. */
  inline def toValidatedNec: ValidatedNec[EmileError, A] =
    either match
      case Right(value) => Validated.validNec(value)
      case Left(errs)   =>
        NonEmptyChain.fromSeq(errs) match
          case Some(nec) => Validated.invalid(nec)
          case None      =>
            Validated.invalidNec(
              EmileError.InvalidArgument("errors", "Empty error list supplied to toValidatedNec.")
            )
end extension
