/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.IO
import io.github.arashi01.emile.EmileError

/**
 * Error handling syntax for cats-effect integration.
 *
 * Provides extension methods to bridge between EmileError and IO's Throwable-based
 * error channel. Since EmileError extends Throwable directly, no wrapper is needed.
 */

extension [A](io: IO[Either[EmileError, A]])
  /**
   * Flatten Either[EmileError, A] into IO error channel.
   *
   * Named `rethrowEmile` to avoid conflict with cats-effect's built-in `rethrow`.
   */
  def rethrowEmile: IO[A] = io.flatMap {
    case Right(a) => IO.pure(a)
    case Left(e)  => IO.raiseError(e)
  }

extension [A](either: Either[EmileError, A])
  /**
   * Lift Either into IO, raising EmileError directly.
   *
   * This is the standard way to convert emile-core results to IO.
   */
  def liftIO: IO[A] = either match
    case Right(a) => IO.pure(a)
    case Left(e)  => IO.raiseError(e)

extension [A](io: IO[A])
  /**
   * Recover from EmileError using partial function.
   *
   * @param pf Partial function from EmileError to recovery action
   */
  def catchEmile(pf: PartialFunction[EmileError, IO[A]]): IO[A] =
    io.handleErrorWith {
      case e: EmileError if pf.isDefinedAt(e) => pf(e)
      case other                              => IO.raiseError(other)
    }

  /**
   * Handle specific EmileError variants.
   *
   * @param f Function from EmileError to recovery value
   */
  def recoverEmile(f: EmileError => A): IO[A] =
    io.handleError {
      case e: EmileError => f(e)
      case other         => throw other
    }

/**
 * Pattern extractor for EmileError in error handlers.
 *
 * Usage:
 * {{{
 * io.handleErrorWith {
 *   case Emile(EmileError.ConnectionRefused) => fallback
 *   case other => IO.raiseError(other)
 * }
 * }}}
 */
object Emile:
  def unapply(t: Throwable): Option[EmileError] = t match
    case e: EmileError => Some(e)
    case _             => None
