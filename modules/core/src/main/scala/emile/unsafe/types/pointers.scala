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
package emile.unsafe.types

// scalafix:off DisableSyntax.null
// scalafix:off DisableSyntax.asInstanceOf

import scala.scalanative.unsafe.Ptr

/** Opaque pointer types for libuv structures.
  *
  * Each type is distinct at compile time, preventing accidental mixing of pointers at the C interop
  * boundary. Companion objects are package-private to `emile`; downstream consumers cannot
  * construct or deconstruct these types.
  */

opaque type UvLoopPtr = Ptr[Byte]

private[emile] object UvLoopPtr:
  given CanEqual[UvLoopPtr, UvLoopPtr] = CanEqual.derived

  inline def apply(p: Ptr[Byte]): UvLoopPtr = p

  extension (p: UvLoopPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null

opaque type UvHandlePtr = Ptr[Byte]

private[emile] object UvHandlePtr:
  inline def apply(p: Ptr[Byte]): UvHandlePtr = p

  extension (p: UvHandlePtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null

opaque type UvStreamPtr = Ptr[Byte]

private[emile] object UvStreamPtr:
  inline def apply(p: Ptr[Byte]): UvStreamPtr = p

  extension (p: UvStreamPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)

opaque type UvTcpPtr = Ptr[Byte]

private[emile] object UvTcpPtr:
  inline def apply(p: Ptr[Byte]): UvTcpPtr = p

  extension (p: UvTcpPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asStream: UvStreamPtr = UvStreamPtr(p)
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)

opaque type UvTimerPtr = Ptr[Byte]

private[emile] object UvTimerPtr:
  given CanEqual[UvTimerPtr, UvTimerPtr] = CanEqual.derived

  val Null: UvTimerPtr = null.asInstanceOf[Ptr[Byte]]

  inline def apply(p: Ptr[Byte]): UvTimerPtr = p

  extension (p: UvTimerPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == Null
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)

opaque type UvAsyncPtr = Ptr[Byte]

private[emile] object UvAsyncPtr:
  given CanEqual[UvAsyncPtr, UvAsyncPtr] = CanEqual.derived

  val Null: UvAsyncPtr = null.asInstanceOf[Ptr[Byte]]

  inline def apply(p: Ptr[Byte]): UvAsyncPtr = p

  extension (p: UvAsyncPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == Null
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)

opaque type UvPollPtr = Ptr[Byte]

private[emile] object UvPollPtr:
  inline def apply(p: Ptr[Byte]): UvPollPtr = p

  extension (p: UvPollPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)

opaque type UvPipePtr = Ptr[Byte]

private[emile] object UvPipePtr:
  inline def apply(p: Ptr[Byte]): UvPipePtr = p

  extension (p: UvPipePtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asStream: UvStreamPtr = UvStreamPtr(p)
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)

opaque type UvReqPtr = Ptr[Byte]

private[emile] object UvReqPtr:
  inline def apply(p: Ptr[Byte]): UvReqPtr = p

  extension (p: UvReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null

opaque type UvWriteReqPtr = Ptr[Byte]

private[emile] object UvWriteReqPtr:
  inline def apply(p: Ptr[Byte]): UvWriteReqPtr = p

  extension (p: UvWriteReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asReq: UvReqPtr = UvReqPtr(p)

opaque type UvConnectReqPtr = Ptr[Byte]

private[emile] object UvConnectReqPtr:
  inline def apply(p: Ptr[Byte]): UvConnectReqPtr = p

  extension (p: UvConnectReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asReq: UvReqPtr = UvReqPtr(p)

opaque type UvShutdownReqPtr = Ptr[Byte]

private[emile] object UvShutdownReqPtr:
  inline def apply(p: Ptr[Byte]): UvShutdownReqPtr = p

  extension (p: UvShutdownReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asReq: UvReqPtr = UvReqPtr(p)

opaque type UvBufPtr = Ptr[Byte]

private[emile] object UvBufPtr:
  inline def apply(p: Ptr[Byte]): UvBufPtr = p

  extension (p: UvBufPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null

opaque type SockAddrPtr = Ptr[Byte]

private[emile] object SockAddrPtr:
  inline def apply(p: Ptr[Byte]): SockAddrPtr = p

  extension (p: SockAddrPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null

opaque type SockAddrInPtr = Ptr[Byte]

private[emile] object SockAddrInPtr:
  inline def apply(p: Ptr[Byte]): SockAddrInPtr = p

  extension (p: SockAddrInPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asSockAddr: SockAddrPtr = SockAddrPtr(p)

opaque type SockAddrIn6Ptr = Ptr[Byte]

private[emile] object SockAddrIn6Ptr:
  inline def apply(p: Ptr[Byte]): SockAddrIn6Ptr = p

  extension (p: SockAddrIn6Ptr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    inline def asSockAddr: SockAddrPtr = SockAddrPtr(p)
