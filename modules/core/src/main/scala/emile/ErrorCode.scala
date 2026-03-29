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
package emile

import boilerplate.*

/** libuv error code, representing negated errno on Unix or libuv-defined codes.
  *
  * Error codes are negative integers in libuv (except UV_EOF which is also negative). Positive
  * values indicate success or the number of bytes transferred. Instances may be constructed via
  * [[ErrorCode$ ErrorCode]].
  */
opaque type ErrorCode = Int

/** Provides factories and extension syntax for [[ErrorCode]]. */
object ErrorCode extends OpaqueType[ErrorCode, Int], OpaqueType.Eq[ErrorCode]:
  type Error = Nothing

  inline def wrap(value: Int): ErrorCode = value
  inline def unwrap(value: ErrorCode): Int = value
  protected inline def validate(value: Int): Option[Nothing] = None
  inline def apply(inline value: Int): ErrorCode = value

  extension (c: ErrorCode)
    /** Raw integer value. */
    inline def value: Int = c

    /** Whether this code indicates success (non-negative). */
    inline def isSuccess: Boolean = c >= 0

    /** Whether this code indicates an error (negative). */
    inline def isError: Boolean = c < 0

    /** Whether this code indicates EOF. */
    inline def isEof: Boolean = c == Eof.value
  end extension

  // Common libuv error codes (negated values from uv/errno.h)
  val Eof: ErrorCode = -4095
  val Cancelled: ErrorCode = -4081
  val ConnectionRefused: ErrorCode = -4078
  val ConnectionReset: ErrorCode = -4077
  val AddressInUse: ErrorCode = -4091
  val AddressNotAvailable: ErrorCode = -4090
  val TimedOut: ErrorCode = -4039
  val InvalidArgument: ErrorCode = -4071
  val BadFileDescriptor: ErrorCode = -4083
  val PermissionDenied: ErrorCode = -4092
  val NetworkUnreachable: ErrorCode = -4056
  val HostUnreachable: ErrorCode = -4062
  val BrokenPipe: ErrorCode = -4047
  val Again: ErrorCode = -4088
  val AlreadyConnected: ErrorCode = -4068
  val NotConnected: ErrorCode = -4053
  val ConnectionAborted: ErrorCode = -4079
  val NoMemory: ErrorCode = -4055
  val Busy: ErrorCode = -4082
  val NoSys: ErrorCode = -4054
  val NotSupported: ErrorCode = -4028
end ErrorCode
