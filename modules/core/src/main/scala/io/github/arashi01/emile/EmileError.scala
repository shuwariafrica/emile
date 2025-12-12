/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.scalanative.unsafe.fromCString
import io.github.arashi01.emile.unsafe.LibUV

/**
 * Root error type for all Emile operations.
 *
 * Extends Throwable with NoStackTrace for zero-overhead exception compatibility.
 * This allows EmileError to be used directly in effect systems (cats-effect IO, ZIO)
 * without requiring a separate wrapper type.
 *
 * All upstream libuv errors and internal exceptions are represented in this ADT.
 * This provides explicit error handling at all call sites.
 */
sealed abstract class EmileError(msg: String)
    extends Exception(msg)
    with scala.util.control.NoStackTrace
    derives CanEqual

object EmileError:
  /**
   * libuv system error with code and message.
   *
   * @param code The libuv error code
   * @param message Human-readable error description from libuv
   */
  final case class SystemError(code: ErrorCode, message: String)
      extends EmileError(message)

  /** Operation was cancelled before completion. */
  case object Cancelled extends EmileError("Operation cancelled")

  /** End of file/stream reached. */
  case object EndOfStream extends EmileError("End of stream")

  /** Resource has already been closed. */
  case object AlreadyClosed extends EmileError("Resource already closed")

  /** Timeout expired before operation completed. */
  case object TimedOut extends EmileError("Operation timed out")

  /**
   * Invalid argument provided to operation.
   *
   * @param name The argument name
   * @param detail Description of why the argument is invalid
   */
  final case class InvalidArgument(name: String, detail: String)
      extends EmileError(s"Invalid argument '$name': $detail")

  /**
   * Port number outside valid range [0, 65535].
   *
   * @param value The invalid port value
   */
  final case class InvalidPort(value: Int)
      extends EmileError(s"Invalid port: $value (must be 0-65535)")

  /**
   * Invalid address format or resolution failure.
   *
   * @param address The invalid address string
   * @param detail Description of the error
   */
  final case class InvalidAddress(address: String, detail: String)
      extends EmileError(s"Invalid address '$address': $detail")

  /**
   * Address resolution failure.
   *
   * @param code The libuv error code
   * @param message Human-readable error description
   */
  final case class AddressError(code: ErrorCode, message: String)
      extends EmileError(message)

  /**
   * Internal error wrapping unexpected exceptions.
   *
   * @param cause The underlying exception
   */
  final case class InternalError(cause: Throwable)
      extends EmileError(s"Internal error: ${cause.getMessage}"):
    override def getCause: Throwable = cause

  /**
   * Multiple errors accumulated from validation.
   *
   * Used when collecting errors in non-fail-fast scenarios.
   *
   * @param errors The accumulated errors
   */
  final case class Combined(errors: List[EmileError])
      extends EmileError(errors.map(_.getMessage).mkString("; "))

  extension (e: EmileError)
    /** Retrieve error code if applicable. */
    def errorCode: Option[ErrorCode] = e match
      case SystemError(c, _)  => Some(c)
      case AddressError(c, _) => Some(c)
      case _                  => None

    /** Lift single error into List for accumulation. */
    def toList: List[EmileError] = List(e)

  /**
   * Create a SystemError from a libuv error code.
   *
   * This is the standard way to convert libuv error codes to EmileError.
   *
   * @param code The libuv error code (negative integer)
   * @return A SystemError with the error code and message from libuv
   */
  def fromErrorCode(code: ErrorCode): SystemError =
    val name = fromCString(LibUV.uv_err_name(code.value))
    val desc = fromCString(LibUV.uv_strerror(code.value))
    SystemError(code, s"$name: $desc")

  /** Memory allocation failure. */
  val OutOfMemory: SystemError = SystemError(ErrorCode.NoMemory, "Out of memory")
end EmileError
