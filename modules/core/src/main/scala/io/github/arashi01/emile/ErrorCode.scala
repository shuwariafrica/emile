/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

/**
 * libuv error code, representing negated errno on Unix or libuv-defined codes.
 *
 * Error codes are negative integers in libuv (except UV_EOF which is also negative).
 * Positive values indicate success or the number of bytes transferred.
 */
opaque type ErrorCode = Int

object ErrorCode:
  given CanEqual[ErrorCode, ErrorCode] = CanEqual.derived

  /** Construct from raw libuv error code. */
  inline def apply(code: Int): ErrorCode = code

  extension (c: ErrorCode)
    /** Raw integer value. */
    inline def value: Int = c

    /** Whether this code indicates success (non-negative). */
    inline def isSuccess: Boolean = c >= 0

    /** Whether this code indicates an error (negative). */
    inline def isError: Boolean = c < 0

    /** Whether this code indicates EOF. */
    inline def isEof: Boolean = c == Eof.value

  // Common libuv error codes (negated values)
  // These match the UV_E* constants from libuv's uv/errno.h
  val Eof: ErrorCode = -4095                    // UV_EOF
  val Cancelled: ErrorCode = -4081              // UV_ECANCELED
  val ConnectionRefused: ErrorCode = -4078      // UV_ECONNREFUSED
  val ConnectionReset: ErrorCode = -4077        // UV_ECONNRESET
  val AddressInUse: ErrorCode = -4091           // UV_EADDRINUSE
  val AddressNotAvailable: ErrorCode = -4090    // UV_EADDRNOTAVAIL
  val TimedOut: ErrorCode = -4039               // UV_ETIMEDOUT
  val InvalidArgument: ErrorCode = -4071        // UV_EINVAL
  val BadFileDescriptor: ErrorCode = -4083      // UV_EBADF
  val PermissionDenied: ErrorCode = -4092       // UV_EACCES
  val NetworkUnreachable: ErrorCode = -4056     // UV_ENETUNREACH
  val HostUnreachable: ErrorCode = -4062        // UV_EHOSTUNREACH
  val BrokenPipe: ErrorCode = -4047             // UV_EPIPE
  val Again: ErrorCode = -4088                  // UV_EAGAIN
  val AlreadyConnected: ErrorCode = -4068       // UV_EISCONN
  val NotConnected: ErrorCode = -4053           // UV_ENOTCONN
  val ConnectionAborted: ErrorCode = -4079      // UV_ECONNABORTED
  val NoMemory: ErrorCode = -4055               // UV_ENOMEM
  val Busy: ErrorCode = -4082                   // UV_EBUSY
  val NoSys: ErrorCode = -4054                  // UV_ENOSYS (function not supported)
  val NotSupported: ErrorCode = -4028           // UV_ENOTSUP (operation not supported)
end ErrorCode
