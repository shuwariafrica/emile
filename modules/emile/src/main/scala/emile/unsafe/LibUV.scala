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
package emile.unsafe

import scala.scalanative.unsafe.*

/** emile's libuv FFI surface (libuv 1.52.1): constants, callback-pointer aliases, and - re-exported
  * from [[LibUVExtern]] - the `@extern` bindings, all reached as `LibUV.*`. The split exists
  * because a Scala Native `@extern` object admits only `extern` declarations. Linkage is
  * build-wired; no `@link`.
  */
private[emile] object LibUV:

  /** `uv_buf_t`. */
  type Buf = CStruct2[Ptr[Byte], CSize]

  /** The leading `uint64_t` fields of `uv_stat_t`; `_8` is `st_size`. */
  type Stat = CStruct8[
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong
  ]

  type AllocCB = CFuncPtr3[Ptr[Byte], CSize, Ptr[Buf], Unit]
  type ReadCB = CFuncPtr3[Ptr[Byte], CSSize, Ptr[Buf], Unit]
  type WriteCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type ConnectCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type ShutdownCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type ConnectionCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type CloseCB = CFuncPtr1[Ptr[Byte], Unit]
  type PollCB = CFuncPtr3[Ptr[Byte], CInt, CInt, Unit]
  type TimerCB = CFuncPtr1[Ptr[Byte], Unit]
  type AsyncCB = CFuncPtr1[Ptr[Byte], Unit]
  type WalkCB = CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]
  type FSCB = CFuncPtr1[Ptr[Byte], Unit]
  type GetAddrInfoCB = CFuncPtr3[Ptr[Byte], CInt, Ptr[Byte], Unit]
  type GetNameInfoCB = CFuncPtr4[Ptr[Byte], CInt, CString, CString, Unit]
  type SignalCB = CFuncPtr2[Ptr[Byte], CInt, Unit]

  // uv_handle_type ordinals (for uv_handle_size).
  inline val UV_ASYNC = 1
  inline val UV_NAMED_PIPE = 7
  inline val UV_POLL = 8
  inline val UV_TCP = 12
  inline val UV_TIMER = 13
  inline val UV_SIGNAL = 16

  // uv_req_type ordinals (for uv_req_size).
  inline val UV_CONNECT = 2
  inline val UV_WRITE = 3
  inline val UV_SHUTDOWN = 4
  inline val UV_FS = 6
  inline val UV_GETADDRINFO = 8
  inline val UV_GETNAMEINFO = 9

  // uv_run_mode.
  inline val UV_RUN_ONCE = 1
  inline val UV_RUN_NOWAIT = 2

  // uv_loop_option.
  inline val UV_LOOP_BLOCK_SIGNAL = 0
  inline val UV_LOOP_USE_IO_URING_SQPOLL = 2

  // uv_poll_event.
  inline val UV_READABLE = 1
  inline val UV_WRITABLE = 2
  inline val UV_DISCONNECT = 4
  inline val UV_PRIORITIZED = 8

  // uv_tcp_flags.
  inline val UV_TCP_IPV6ONLY = 1
  inline val UV_TCP_REUSEPORT = 2

  // uv_fs open flag.
  inline val UV_FS_O_RDONLY = 0

  export LibUVExtern.*

end LibUV

/** Raw `@extern` libuv bindings; reached through [[LibUV]]. */
@extern
private[unsafe] object LibUVExtern:

  def uv_loop_init(loop: Ptr[Byte]): CInt = extern
  def uv_loop_close(loop: Ptr[Byte]): CInt = extern
  def uv_loop_alive(loop: Ptr[Byte]): CInt = extern
  def uv_loop_configure(loop: Ptr[Byte], option: CInt, args: Any*): CInt = extern
  def uv_loop_size(): CSize = extern
  // @blocking: UV_RUN_ONCE / DEFAULT block in epoll_wait; without it the SN GC cannot stop the
  // worker thread for a collection.
  @blocking def uv_run(loop: Ptr[Byte], mode: CInt): CInt = extern
  def uv_walk(loop: Ptr[Byte], walkCb: LibUV.WalkCB, arg: Ptr[Byte]): Unit = extern

  def uv_handle_size(handleType: CInt): CSize = extern
  def uv_handle_get_data(handle: Ptr[Byte]): Ptr[Byte] = extern
  def uv_handle_set_data(handle: Ptr[Byte], data: Ptr[Byte]): Unit = extern
  def uv_close(handle: Ptr[Byte], closeCb: LibUV.CloseCB): Unit = extern
  def uv_is_closing(handle: Ptr[Byte]): CInt = extern
  def uv_fileno(handle: Ptr[Byte], fd: Ptr[CInt]): CInt = extern

  def uv_req_size(reqType: CInt): CSize = extern
  def uv_req_get_data(req: Ptr[Byte]): Ptr[Byte] = extern
  def uv_req_set_data(req: Ptr[Byte], data: Ptr[Byte]): Unit = extern
  def uv_cancel(req: Ptr[Byte]): CInt = extern

  def uv_async_init(loop: Ptr[Byte], handle: Ptr[Byte], asyncCb: LibUV.AsyncCB): CInt = extern
  def uv_async_send(handle: Ptr[Byte]): CInt = extern

  def uv_timer_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern
  def uv_timer_start(
    handle: Ptr[Byte],
    timerCb: LibUV.TimerCB,
    timeout: CUnsignedLongLong,
    repeat: CUnsignedLongLong
  ): CInt = extern
  def uv_timer_stop(handle: Ptr[Byte]): CInt = extern

  def uv_listen(stream: Ptr[Byte], backlog: CInt, connectionCb: LibUV.ConnectionCB): CInt = extern
  def uv_accept(server: Ptr[Byte], client: Ptr[Byte]): CInt = extern
  def uv_read_start(stream: Ptr[Byte], allocCb: LibUV.AllocCB, readCb: LibUV.ReadCB): CInt = extern
  def uv_read_stop(stream: Ptr[Byte]): CInt = extern
  def uv_write(
    req: Ptr[Byte],
    handle: Ptr[Byte],
    bufs: Ptr[LibUV.Buf],
    nbufs: CUnsignedInt,
    writeCb: LibUV.WriteCB
  ): CInt = extern
  def uv_shutdown(req: Ptr[Byte], handle: Ptr[Byte], shutdownCb: LibUV.ShutdownCB): CInt = extern
  def uv_try_write(handle: Ptr[Byte], bufs: Ptr[LibUV.Buf], nbufs: CUnsignedInt): CInt = extern

  def uv_tcp_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern
  def uv_tcp_bind(handle: Ptr[Byte], addr: Ptr[Byte], flags: CUnsignedInt): CInt = extern
  def uv_tcp_connect(req: Ptr[Byte], handle: Ptr[Byte], addr: Ptr[Byte], connectCb: LibUV.ConnectCB): CInt = extern
  def uv_tcp_nodelay(handle: Ptr[Byte], enable: CInt): CInt = extern
  def uv_tcp_keepalive_ex(
    handle: Ptr[Byte],
    on: CInt,
    idle: CUnsignedInt,
    intvl: CUnsignedInt,
    cnt: CUnsignedInt
  ): CInt = extern
  def uv_tcp_simultaneous_accepts(handle: Ptr[Byte], enable: CInt): CInt = extern
  def uv_tcp_getsockname(handle: Ptr[Byte], name: Ptr[Byte], nameLen: Ptr[CInt]): CInt = extern
  def uv_tcp_getpeername(handle: Ptr[Byte], name: Ptr[Byte], nameLen: Ptr[CInt]): CInt = extern

  // uv_pipe_t (a uv_stream_t): Unix-domain sockets on Unix, named pipes on Windows.
  def uv_pipe_init(loop: Ptr[Byte], handle: Ptr[Byte], ipc: CInt): CInt = extern
  def uv_pipe_bind2(handle: Ptr[Byte], name: CString, namelen: CSize, flags: CUnsignedInt): CInt = extern
  def uv_pipe_connect2(
    req: Ptr[Byte],
    handle: Ptr[Byte],
    name: CString,
    namelen: CSize,
    flags: CUnsignedInt,
    connectCb: LibUV.ConnectCB
  ): CInt = extern
  def uv_pipe_getsockname(handle: Ptr[Byte], buffer: CString, size: Ptr[CSize]): CInt = extern
  def uv_pipe_getpeername(handle: Ptr[Byte], buffer: CString, size: Ptr[CSize]): CInt = extern
  def uv_pipe_chmod(handle: Ptr[Byte], flags: CInt): CInt = extern

  def uv_poll_init(loop: Ptr[Byte], handle: Ptr[Byte], fd: CInt): CInt = extern
  def uv_poll_start(handle: Ptr[Byte], events: CInt, pollCb: LibUV.PollCB): CInt = extern
  def uv_poll_stop(handle: Ptr[Byte]): CInt = extern

  def uv_signal_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern
  def uv_signal_start(handle: Ptr[Byte], signalCb: LibUV.SignalCB, signum: CInt): CInt = extern
  def uv_signal_stop(handle: Ptr[Byte]): CInt = extern

  def uv_getaddrinfo(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    getAddrInfoCb: LibUV.GetAddrInfoCB,
    node: CString,
    service: CString,
    hints: Ptr[Byte]
  ): CInt = extern
  def uv_freeaddrinfo(ai: Ptr[Byte]): Unit = extern
  def uv_getnameinfo(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    getNameInfoCb: LibUV.GetNameInfoCB,
    addr: Ptr[Byte],
    flags: CInt
  ): CInt = extern

  def uv_fs_open(loop: Ptr[Byte], req: Ptr[Byte], path: CString, flags: CInt, mode: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_close(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, cb: LibUV.FSCB): CInt = extern
  // A negative offset reads from (and advances) the current file position; the bufs array is copied,
  // so it need not outlive the call - but each buffer's memory must, until the callback fires.
  def uv_fs_read(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    file: CInt,
    bufs: Ptr[LibUV.Buf],
    nbufs: CUnsignedInt,
    offset: CLongLong,
    cb: LibUV.FSCB
  ): CInt = extern
  def uv_fs_fstat(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_sendfile(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    outFd: CInt,
    inFd: CInt,
    inOffset: CLongLong,
    length: CSize,
    cb: LibUV.FSCB
  ): CInt = extern
  def uv_fs_req_cleanup(req: Ptr[Byte]): Unit = extern
  def uv_fs_get_result(req: Ptr[Byte]): CSSize = extern
  def uv_fs_get_statbuf(req: Ptr[Byte]): Ptr[LibUV.Stat] = extern

  def uv_ip4_addr(ip: CString, port: CInt, addr: Ptr[Byte]): CInt = extern
  def uv_ip6_addr(ip: CString, port: CInt, addr: Ptr[Byte]): CInt = extern
  def uv_ip4_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern
  def uv_ip6_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern

  def uv_strerror_r(err: CInt, buf: CString, buflen: CSize): CString = extern
  def uv_err_name_r(err: CInt, buf: CString, buflen: CSize): CString = extern
  def uv_version(): CUnsignedInt = extern

end LibUVExtern
