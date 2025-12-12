/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import zio.*
import io.github.arashi01.emile.EmileError

/**
 * Error handling syntax for ZIO integration.
 *
 * Provides extension methods to bridge between EmileError and ZIO's typed
 * error channel. Since EmileError extends Throwable directly, it can also
 * be used with ZIO's defect handling when needed.
 *
 * Extensions are defined at top-level for automatic resolution when
 * importing the package.
 */

/**
 * Extension for lifting Either[EmileError, A] into ZIO.
 */
extension [A](either: Either[EmileError, A])
  /**
   * Lift Either into ZIO error channel.
   *
   * This is the standard way to convert emile-core results to ZIO.
   */
  inline def toZIO: IO[EmileError, A] = ZIO.fromEither(either)

/**
 * Extensions for ZIO with EmileError error type.
 */
extension [R, A](zio: ZIO[R, EmileError, A])
  /**
   * Recover from specific EmileError variants using partial function.
   *
   * @param pf Partial function from EmileError to recovery action
   */
  inline def catchEmile[A1 >: A](pf: PartialFunction[EmileError, ZIO[R, EmileError, A1]]): ZIO[R, EmileError, A1] =
    zio.catchSome(pf)

  /**
   * Handle all EmileError variants.
   *
   * @param f Function from EmileError to recovery value
   */
  inline def recoverEmile[A1 >: A](f: EmileError => A1): ZIO[R, Nothing, A1] =
    zio.fold(f, identity)

  /**
   * Transform EmileError to a different error type.
   *
   * @param f Function from EmileError to new error type
   */
  inline def mapEmileError[E2](f: EmileError => E2): ZIO[R, E2, A] =
    zio.mapError(f)

/**
 * Pattern extractor for EmileError in error handlers.
 *
 * Usage:
 * {{{
 * zio.catchAll:
 *   case Emile(EmileError.AlreadyClosed) => fallback
 *   case other => ZIO.fail(other)
 * }}}
 */
object Emile:
  def unapply(t: Throwable): Option[EmileError] = t match
    case e: EmileError => Some(e)
    case _             => None
