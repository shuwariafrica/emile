/*
 * Copyright 2025, 2026 Ali Rashid
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

import scala.scalanative.posix.errno
import scala.scalanative.unsafe.CChar
import scala.scalanative.unsafe.fromCString
import scala.scalanative.unsafe.stackalloc
import scala.scalanative.unsigned.*

import emile.unsafe.LibUV

/** A libuv error code - the raw negative `uv_errno` integer returned by a failed `uv_*` call.
  * Constructed and inspected through [[ErrorCode$ ErrorCode]].
  */
opaque type ErrorCode = Int

/** Factory and accessors for [[ErrorCode]], and the libuv error-code constants the `EmileError`
  * dispatch tables match against.
  */
object ErrorCode:

  /** Wraps a raw libuv error integer. */
  inline def apply(code: Int): ErrorCode = code

  // Buffer for the reentrant uv_*_r calls; libuv's longest error name or
  // message is comfortably under this.
  private inline val DescribeBufferSize = 256

  given CanEqual[ErrorCode, ErrorCode] = CanEqual.derived

  extension (code: ErrorCode)
    /** The raw libuv error integer. */
    inline def value: Int = code

    // _r forms required: the non-_r uv_err_name / uv_strerror leak a uv__strdup heap copy for
    // codes outside UV_ERRNO_MAP. fromCString copies into a heap String before the stack buffer
    // goes out of scope.

    /** The libuv error name, e.g. `EADDRINUSE`. */
    def errnoName: String =
      val buf = stackalloc[CChar](DescribeBufferSize)
      fromCString(LibUV.uv_err_name_r(code, buf, DescribeBufferSize.toCSize))

    /** The libuv error message, e.g. `address already in use`. */
    def errnoMessage: String =
      val buf = stackalloc[CChar](DescribeBufferSize)
      fromCString(LibUV.uv_strerror_r(code, buf, DescribeBufferSize.toCSize))
  end extension

  /** The libuv error name and message together, e.g. `EADDRINUSE: address already in use`. */
  def describe(code: ErrorCode): String =
    s"${code.errnoName}: ${code.errnoMessage}"

  // Errno-backed libuv codes are the negated platform errno on Linux (libuv
  // defines `UV__ERR(x)` as `-x`). Plain `val`, not `inline val`: `errno.E*`
  // is a link-time extern, not a compile-time constant - but a stable `val`
  // is a valid match pattern, which is all the `*Mapping` tables require.
  val UV_EADDRINUSE: Int = -errno.EADDRINUSE
  val UV_EADDRNOTAVAIL: Int = -errno.EADDRNOTAVAIL
  val UV_EACCES: Int = -errno.EACCES
  val UV_ECONNREFUSED: Int = -errno.ECONNREFUSED
  val UV_ENETUNREACH: Int = -errno.ENETUNREACH
  val UV_EHOSTUNREACH: Int = -errno.EHOSTUNREACH
  val UV_ETIMEDOUT: Int = -errno.ETIMEDOUT
  val UV_ECONNRESET: Int = -errno.ECONNRESET
  val UV_EPIPE: Int = -errno.EPIPE
  val UV_EAGAIN: Int = -errno.EAGAIN

  // libuv pseudo-codes with no errno - fixed on every platform, hence literals.
  inline val UV_EOF = -4095
  inline val UV_EAI_NONAME = -3008
  inline val UV_EAI_NODATA = -3007
  inline val UV_EAI_AGAIN = -3001

end ErrorCode
