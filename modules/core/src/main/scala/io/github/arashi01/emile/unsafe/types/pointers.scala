/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.unsafe.types

import scala.scalanative.unsafe.Ptr

/**
 * Opaque pointer types for libuv structures.
 *
 * Each type is distinct at compile time, preventing accidental mixing
 * of pointers at the C interop boundary.
 *
 * INTERNAL: These types are not part of the public API.
 */

/** Raw pointer to uv_loop_t structure. */
opaque type UvLoopPtr = Ptr[Byte]

object UvLoopPtr:
  given CanEqual[UvLoopPtr, UvLoopPtr] = CanEqual.derived

  inline def apply(p: Ptr[Byte]): UvLoopPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvLoopPtr = p

  extension (p: UvLoopPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
end UvLoopPtr

/** Raw pointer to uv_handle_t structure (base handle). */
opaque type UvHandlePtr = Ptr[Byte]

object UvHandlePtr:
  inline def apply(p: Ptr[Byte]): UvHandlePtr = p
  inline def fromPtr(p: Ptr[Byte]): UvHandlePtr = p

  extension (p: UvHandlePtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
end UvHandlePtr

/** Raw pointer to uv_stream_t structure (base stream). */
opaque type UvStreamPtr = Ptr[Byte]

object UvStreamPtr:
  inline def apply(p: Ptr[Byte]): UvStreamPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvStreamPtr = p

  extension (p: UvStreamPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to handle pointer. */
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)
end UvStreamPtr

/** Raw pointer to uv_tcp_t structure. */
opaque type UvTcpPtr = Ptr[Byte]

object UvTcpPtr:
  inline def apply(p: Ptr[Byte]): UvTcpPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvTcpPtr = p

  extension (p: UvTcpPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to stream pointer. */
    inline def asStream: UvStreamPtr = UvStreamPtr(p)
    /** Safe upcast to handle pointer. */
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)
end UvTcpPtr

/** Raw pointer to uv_timer_t structure. */
opaque type UvTimerPtr = Ptr[Byte]

object UvTimerPtr:
  inline def apply(p: Ptr[Byte]): UvTimerPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvTimerPtr = p

  extension (p: UvTimerPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to handle pointer. */
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)
end UvTimerPtr

/** Raw pointer to uv_async_t structure. */
opaque type UvAsyncPtr = Ptr[Byte]

object UvAsyncPtr:
  given CanEqual[UvAsyncPtr, UvAsyncPtr] = CanEqual.derived

  /** Null async pointer constant. */
  val Null: UvAsyncPtr = null.asInstanceOf[Ptr[Byte]]

  inline def apply(p: Ptr[Byte]): UvAsyncPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvAsyncPtr = p

  extension (p: UvAsyncPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == Null
    /** Safe upcast to handle pointer. */
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)
end UvAsyncPtr

/** Raw pointer to uv_poll_t structure. */
opaque type UvPollPtr = Ptr[Byte]

object UvPollPtr:
  inline def apply(p: Ptr[Byte]): UvPollPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvPollPtr = p

  extension (p: UvPollPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to handle pointer. */
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)
end UvPollPtr

/** Raw pointer to uv_pipe_t structure. */
opaque type UvPipePtr = Ptr[Byte]

object UvPipePtr:
  inline def apply(p: Ptr[Byte]): UvPipePtr = p
  inline def fromPtr(p: Ptr[Byte]): UvPipePtr = p

  extension (p: UvPipePtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to stream pointer. */
    inline def asStream: UvStreamPtr = UvStreamPtr(p)
    /** Safe upcast to handle pointer. */
    inline def asHandle: UvHandlePtr = UvHandlePtr(p)
end UvPipePtr

/** Raw pointer to uv_req_t structure (base request). */
opaque type UvReqPtr = Ptr[Byte]

object UvReqPtr:
  inline def apply(p: Ptr[Byte]): UvReqPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvReqPtr = p

  extension (p: UvReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
end UvReqPtr

/** Raw pointer to uv_write_t structure. */
opaque type UvWriteReqPtr = Ptr[Byte]

object UvWriteReqPtr:
  inline def apply(p: Ptr[Byte]): UvWriteReqPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvWriteReqPtr = p

  extension (p: UvWriteReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to request pointer. */
    inline def asReq: UvReqPtr = UvReqPtr(p)
end UvWriteReqPtr

/** Raw pointer to uv_connect_t structure. */
opaque type UvConnectReqPtr = Ptr[Byte]

object UvConnectReqPtr:
  inline def apply(p: Ptr[Byte]): UvConnectReqPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvConnectReqPtr = p

  extension (p: UvConnectReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to request pointer. */
    inline def asReq: UvReqPtr = UvReqPtr(p)
end UvConnectReqPtr

/** Raw pointer to uv_shutdown_t structure. */
opaque type UvShutdownReqPtr = Ptr[Byte]

object UvShutdownReqPtr:
  inline def apply(p: Ptr[Byte]): UvShutdownReqPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvShutdownReqPtr = p

  extension (p: UvShutdownReqPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to request pointer. */
    inline def asReq: UvReqPtr = UvReqPtr(p)
end UvShutdownReqPtr

/** Raw pointer to uv_buf_t structure. */
opaque type UvBufPtr = Ptr[Byte]

object UvBufPtr:
  inline def apply(p: Ptr[Byte]): UvBufPtr = p
  inline def fromPtr(p: Ptr[Byte]): UvBufPtr = p

  extension (p: UvBufPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
end UvBufPtr

/** Raw pointer to sockaddr structure. */
opaque type SockAddrPtr = Ptr[Byte]

object SockAddrPtr:
  inline def apply(p: Ptr[Byte]): SockAddrPtr = p
  inline def fromPtr(p: Ptr[Byte]): SockAddrPtr = p

  extension (p: SockAddrPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
end SockAddrPtr

/** Raw pointer to sockaddr_in (IPv4) structure. */
opaque type SockAddrInPtr = Ptr[Byte]

object SockAddrInPtr:
  inline def apply(p: Ptr[Byte]): SockAddrInPtr = p
  inline def fromPtr(p: Ptr[Byte]): SockAddrInPtr = p

  extension (p: SockAddrInPtr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to generic sockaddr pointer. */
    inline def asSockAddr: SockAddrPtr = SockAddrPtr(p)
end SockAddrInPtr

/** Raw pointer to sockaddr_in6 (IPv6) structure. */
opaque type SockAddrIn6Ptr = Ptr[Byte]

object SockAddrIn6Ptr:
  inline def apply(p: Ptr[Byte]): SockAddrIn6Ptr = p
  inline def fromPtr(p: Ptr[Byte]): SockAddrIn6Ptr = p

  extension (p: SockAddrIn6Ptr)
    inline def ptr: Ptr[Byte] = p
    inline def isNull: Boolean = p == null
    /** Safe upcast to generic sockaddr pointer. */
    inline def asSockAddr: SockAddrPtr = SockAddrPtr(p)
end SockAddrIn6Ptr
