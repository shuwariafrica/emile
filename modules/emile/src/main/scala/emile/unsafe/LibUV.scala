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

/** emile's libuv FFI surface (libuv >= 1.51): constants, callback-pointer aliases, and -
  * re-exported from [[LibUVExtern]] - the `@extern` bindings, all reached as `LibUV.*`. The split
  * exists because a Scala Native `@extern` object admits only `extern` declarations. Linkage is
  * build-wired; no `@link`.
  */
private[emile] object LibUV:

  /** `uv_buf_t`. */
  type Buf = CStruct2[Ptr[Byte], CSize]

  /** `uv_timespec_t` - `long`-based seconds and nanoseconds (LP64 today, ILP32 width-sensitive). */
  type Timespec = CStruct2[CLong, CLong]

  /** `uv_stat_t`, all sixteen fields: twelve `uint64_t` (`_8` is `st_size`) then the atime, mtime,
    * ctime, and birthtime [[Timespec]]s (`.at13`..`.at16`).
    */
  type Stat = CStruct16[
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    Timespec,
    Timespec,
    Timespec,
    Timespec
  ]

  /** The leading eight `uint64_t` fields of `uv_statfs_t` (`f_type` .. `f_frsize`); the
    * `f_spare[3]` tail is unread.
    */
  type Statfs = CStruct8[
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong,
    CUnsignedLongLong
  ]

  /** `uv_dirent_t`: a strdup'd `name` (freed by `uv_fs_req_cleanup`) and a `uv_dirent_type_t`
    * ordinal.
    */
  type Dirent = CStruct2[CString, CInt]

  /** The prefix of `uv_dir_t` the readdir protocol writes: the caller-owned `dirents` array and its
    * capacity `nentries`, both set before every `uv_fs_readdir`.
    */
  type Dir = CStruct2[Ptr[Dirent], CSize]

  /** `uv_interface_address_t` (80 bytes): name, the 6-byte hardware address then its alignment pad,
    * `is_internal`, and the address and netmask `sockaddr` unions (`.at5` / `.at6`).
    */
  type IfAddr = CStruct6[
    Ptr[Byte],
    CArray[Byte, Nat._6],
    CShort,
    CInt,
    CArray[Byte, Nat.Digit2[Nat._2, Nat._8]],
    CArray[Byte, Nat.Digit2[Nat._3, Nat._2]]
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
  // uv_random_cb: (req, status, buf, buflen). The buf / buflen echo the fill target and are unused.
  type RandomCB = CFuncPtr4[Ptr[Byte], CInt, Ptr[Byte], CSize, Unit]
  type GetAddrInfoCB = CFuncPtr3[Ptr[Byte], CInt, Ptr[Byte], Unit]
  type GetNameInfoCB = CFuncPtr4[Ptr[Byte], CInt, CString, CString, Unit]
  type SignalCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type FSEventCB = CFuncPtr4[Ptr[Byte], CString, CInt, CInt, Unit]
  // The prev / curr uv_stat_t pointers are unused: only the status discriminates appeared/changed.
  type FSPollCB = CFuncPtr4[Ptr[Byte], CInt, Ptr[Byte], Ptr[Byte], Unit]
  // uv_exit_cb: (process, int64 exit_status, int term_signal). exit_status carries the 8-bit wait code,
  // term_signal the terminating signal; they are mutually exclusive (one is zero).
  type ExitCB = CFuncPtr3[Ptr[Byte], CLongLong, CInt, Unit]

  /** `uv_stdio_container_t`: `flags` at offset 0, the `union { uv_stream_t* stream; int fd; }` at
    * offset 8. A create-pipe slot sets `_2` to the parent `uv_pipe_t`; an inherit-fd slot writes
    * the int into the union's low bytes by reinterpreting `_2` as `Ptr[CInt]`.
    */
  type StdioContainer = CStruct2[CInt, Ptr[Byte]]

  /** `uv_process_options_t`, field-for-field (64 bytes, every field naturally aligned on x86_64
    * LP64): exit_cb, file, args, env, cwd, flags, stdio_count, stdio, uid, gid.
    */
  type ProcessOptions = CStruct10[
    ExitCB,
    CString,
    Ptr[CString],
    Ptr[CString],
    CString,
    CUnsignedInt,
    CInt,
    Ptr[StdioContainer],
    CUnsignedInt,
    CUnsignedInt
  ]

  // uv_handle_type ordinals (for uv_handle_size, and the values uv_guess_handle returns).
  inline val UV_ASYNC = 1
  inline val UV_FS_EVENT = 3
  inline val UV_FS_POLL = 4
  inline val UV_NAMED_PIPE = 7
  inline val UV_POLL = 8
  inline val UV_PROCESS = 10
  inline val UV_TCP = 12
  inline val UV_TIMER = 13
  inline val UV_TTY = 14
  inline val UV_UDP = 15
  inline val UV_SIGNAL = 16
  inline val UV_FILE = 17

  // uv_tty_mode_t: the two modes emile drives - NORMAL (cooked) and RAW_VT (raw input; on Unix
  // RAW_VT collapses to the single raw mode, on Windows it adds ENABLE_VIRTUAL_TERMINAL_INPUT).
  inline val UV_TTY_MODE_NORMAL = 0
  inline val UV_TTY_MODE_RAW_VT = 3

  // uv_req_type ordinals (for uv_req_size).
  inline val UV_CONNECT = 2
  inline val UV_WRITE = 3
  inline val UV_SHUTDOWN = 4
  inline val UV_FS = 6
  inline val UV_GETADDRINFO = 8
  inline val UV_GETNAMEINFO = 9
  inline val UV_RANDOM = 10

  // uv_run_mode.
  inline val UV_RUN_ONCE = 1
  inline val UV_RUN_NOWAIT = 2

  // uv_loop_option.
  inline val UV_LOOP_BLOCK_SIGNAL = 0
  inline val UV_LOOP_USE_IO_URING_SQPOLL = 2

  // uv_poll_event. UV_READABLE / UV_WRITABLE double as the uv_pipe_chmod access flags.
  inline val UV_READABLE = 1
  inline val UV_WRITABLE = 2
  inline val UV_DISCONNECT = 4
  inline val UV_PRIORITIZED = 8

  // uv_pipe_bind2 / uv_pipe_connect2 flag: reject a name longer than sun_path rather than truncating
  // it to a different socket.
  inline val UV_PIPE_NO_TRUNCATE = 1

  // uv_fs_event.
  inline val UV_RENAME = 1
  inline val UV_CHANGE = 2

  // uv_tcp_flags.
  inline val UV_TCP_IPV6ONLY = 1
  inline val UV_TCP_REUSEPORT = 2

  // uv_fs open flag. The write / create O_* set is not fixed across platforms, so it is sourced from
  // the posix layer at the call site (OpenMode.flags) rather than bound here; RDONLY is universally 0.
  inline val UV_FS_O_RDONLY = 0

  // uv_fs_copyfile flags (libuv-fixed, not platform O_*): EXCL fails if the destination exists,
  // FICLONE attempts a reflink with a byte-copy fallback, FICLONE_FORCE reflinks or errors.
  inline val UV_FS_COPYFILE_EXCL = 0x1
  inline val UV_FS_COPYFILE_FICLONE = 0x2
  inline val UV_FS_COPYFILE_FICLONE_FORCE = 0x4

  // uv_stdio_flags. The direction flags are the child's perspective; UV_READABLE_PIPE means the child
  // reads (parent writes), UV_WRITABLE_PIPE the child writes (parent reads), both together duplex.
  // UV_NONBLOCK_PIPE (0x40) is deliberately absent: the Unix uv_spawn stdio path ignores it.
  inline val UV_IGNORE = 0x00
  inline val UV_CREATE_PIPE = 0x01
  inline val UV_INHERIT_FD = 0x02
  inline val UV_READABLE_PIPE = 0x10
  inline val UV_WRITABLE_PIPE = 0x20

  // uv_process_flags emile drives. SETUID / SETGID gate the uid / gid fields; DETACHED makes the child
  // a session leader whose loop reference is dropped by uv_unref. The Windows-only flags are omitted.
  inline val UV_PROCESS_SETUID = 0x01
  inline val UV_PROCESS_SETGID = 0x02
  inline val UV_PROCESS_DETACHED = 0x08

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
  // Sets SO_KEEPALIVE and TCP_KEEPIDLE (from `delay`, in seconds); the probe interval and count are
  // set separately by setsockopt so the floor stays at libuv 1.51 (uv_tcp_keepalive_ex is 1.52).
  def uv_tcp_keepalive(handle: Ptr[Byte], enable: CInt, delay: CUnsignedInt): CInt = extern
  def uv_tcp_simultaneous_accepts(handle: Ptr[Byte], enable: CInt): CInt = extern
  def uv_tcp_getsockname(handle: Ptr[Byte], name: Ptr[Byte], nameLen: Ptr[CInt]): CInt = extern
  def uv_tcp_getpeername(handle: Ptr[Byte], name: Ptr[Byte], nameLen: Ptr[CInt]): CInt = extern
  // Abortive close: sends a TCP RST (via SO_LINGER 0) and uv_close's the handle, so the close callback
  // still frees it. Returns < 0 without closing if a uv_shutdown is pending (mixing is disallowed).
  def uv_tcp_close_reset(handle: Ptr[Byte], closeCb: LibUV.CloseCB): CInt = extern

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

  // uv_process_t: uv_spawn forks and execs, returning the exec errno synchronously (exit_cb never fires
  // on failure). uv_unref drops the handle's loop reference for a detached child; uv_process_kill /
  // uv_kill send a signal by handle / pid; uv_disable_stdio_inheritance is a best-effort CLOEXEC sweep.
  def uv_spawn(loop: Ptr[Byte], handle: Ptr[Byte], options: Ptr[LibUV.ProcessOptions]): CInt = extern
  def uv_process_kill(handle: Ptr[Byte], signum: CInt): CInt = extern
  def uv_kill(pid: CInt, signum: CInt): CInt = extern
  def uv_process_get_pid(handle: Ptr[Byte]): CInt = extern
  def uv_unref(handle: Ptr[Byte]): Unit = extern
  def uv_disable_stdio_inheritance(): Unit = extern

  // uv_tty_t (a uv_stream_t): a terminal handle. The 4th uv_tty_init parameter is deprecated and
  // ignored since 1.23.1 (direction is auto-detected from the fd); a non-tty fd yields UV_EINVAL.
  // uv_tty_reset_mode restores the first raw tty's termios and is async-signal-safe, so it is the
  // crash-path restore. uv_guess_handle classifies a bare fd with no loop or handle.
  def uv_guess_handle(fd: CInt): CInt = extern
  def uv_tty_init(loop: Ptr[Byte], handle: Ptr[Byte], fd: CInt, unused: CInt): CInt = extern
  def uv_tty_set_mode(handle: Ptr[Byte], mode: CInt): CInt = extern
  def uv_tty_reset_mode(): CInt = extern
  def uv_tty_get_winsize(handle: Ptr[Byte], width: Ptr[CInt], height: Ptr[CInt]): CInt = extern

  // uv_fs_event_t: a path-change watcher (inotify on Linux). uv_close stops and frees it, so no
  // separate stop binding is needed.
  def uv_fs_event_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern
  def uv_fs_event_start(handle: Ptr[Byte], cb: LibUV.FSEventCB, path: CString, flags: CUnsignedInt): CInt = extern

  // uv_fs_poll_t: a stat-polling path watcher for backends inotify cannot serve (NFS, some containers).
  // uv_close runs uv__fs_poll_close, which stops and frees it, so no separate stop binding is needed.
  def uv_fs_poll_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern
  def uv_fs_poll_start(handle: Ptr[Byte], cb: LibUV.FSPollCB, path: CString, interval: CUnsignedInt): CInt = extern

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
  // req->ptr: the READLINK / REALPATH string or the STATFS struct (freed by uv_fs_req_cleanup, so read
  // first); req->path: the MKDTEMP / MKSTEMP created path (always allocated, freed by cleanup).
  def uv_fs_get_ptr(req: Ptr[Byte]): Ptr[Byte] = extern
  def uv_fs_get_path(req: Ptr[Byte]): CString = extern

  // A negative offset writes at (and advances) the current file position, a non-negative offset is a
  // positioned write that does not move it; under O_APPEND the kernel ignores the offset. The bufs
  // array is copied, but each buffer's memory must live until the callback fires. A short count can
  // occur with a late error swallowed, so the caller resubmits the remainder to complete or surface it.
  def uv_fs_write(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    file: CInt,
    bufs: Ptr[LibUV.Buf],
    nbufs: CUnsignedInt,
    offset: CLongLong,
    cb: LibUV.FSCB
  ): CInt = extern
  def uv_fs_ftruncate(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, offset: CLongLong, cb: LibUV.FSCB): CInt = extern
  def uv_fs_fsync(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_fdatasync(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_fchmod(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, mode: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_futime(loop: Ptr[Byte], req: Ptr[Byte], file: CInt, atime: CDouble, mtime: CDouble, cb: LibUV.FSCB): CInt = extern
  def uv_fs_fchown(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    file: CInt,
    uid: CUnsignedInt,
    gid: CUnsignedInt,
    cb: LibUV.FSCB
  ): CInt = extern

  def uv_fs_stat(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_lstat(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_statfs(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_realpath(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_access(loop: Ptr[Byte], req: Ptr[Byte], path: CString, mode: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_chmod(loop: Ptr[Byte], req: Ptr[Byte], path: CString, mode: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_utime(loop: Ptr[Byte], req: Ptr[Byte], path: CString, atime: CDouble, mtime: CDouble, cb: LibUV.FSCB): CInt = extern
  def uv_fs_lutime(loop: Ptr[Byte], req: Ptr[Byte], path: CString, atime: CDouble, mtime: CDouble, cb: LibUV.FSCB): CInt =
    extern
  def uv_fs_chown(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    path: CString,
    uid: CUnsignedInt,
    gid: CUnsignedInt,
    cb: LibUV.FSCB
  ): CInt = extern
  def uv_fs_lchown(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    path: CString,
    uid: CUnsignedInt,
    gid: CUnsignedInt,
    cb: LibUV.FSCB
  ): CInt = extern

  def uv_fs_unlink(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_rename(loop: Ptr[Byte], req: Ptr[Byte], path: CString, newPath: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_copyfile(loop: Ptr[Byte], req: Ptr[Byte], path: CString, newPath: CString, flags: CInt, cb: LibUV.FSCB): CInt =
    extern
  def uv_fs_link(loop: Ptr[Byte], req: Ptr[Byte], path: CString, newPath: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_symlink(loop: Ptr[Byte], req: Ptr[Byte], path: CString, newPath: CString, flags: CInt, cb: LibUV.FSCB): CInt =
    extern
  def uv_fs_readlink(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_mkdir(loop: Ptr[Byte], req: Ptr[Byte], path: CString, mode: CInt, cb: LibUV.FSCB): CInt = extern
  def uv_fs_rmdir(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_mkdtemp(loop: Ptr[Byte], req: Ptr[Byte], tpl: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_mkstemp(loop: Ptr[Byte], req: Ptr[Byte], tpl: CString, cb: LibUV.FSCB): CInt = extern

  // opendir delivers the uv_dir_t at req->ptr (NOT freed by cleanup - closedir frees it). readdir fills
  // up to dir->nentries dirents (result is the count, 0 = end); its strdup'd names are freed by
  // uv_fs_req_cleanup, which must run before the next readdir or closedir.
  def uv_fs_opendir(loop: Ptr[Byte], req: Ptr[Byte], path: CString, cb: LibUV.FSCB): CInt = extern
  def uv_fs_readdir(loop: Ptr[Byte], req: Ptr[Byte], dir: Ptr[Byte], cb: LibUV.FSCB): CInt = extern
  def uv_fs_closedir(loop: Ptr[Byte], req: Ptr[Byte], dir: Ptr[Byte], cb: LibUV.FSCB): CInt = extern

  // Fills buf with buflen cryptographically strong bytes from the system CSPRNG; async (cb != NULL)
  // runs on the threadpool. flags must be 0. The buffer must live until the callback fires.
  def uv_random(
    loop: Ptr[Byte],
    req: Ptr[Byte],
    buf: Ptr[Byte],
    buflen: CSize,
    flags: CUnsignedInt,
    cb: LibUV.RandomCB
  ): CInt = extern

  // One contiguous block holds the address array and the name strings; uv_free_interface_addresses
  // frees it in a single call (count ignored on Unix).
  def uv_interface_addresses(addresses: Ptr[Ptr[LibUV.IfAddr]], count: Ptr[CInt]): CInt = extern
  def uv_free_interface_addresses(addresses: Ptr[LibUV.IfAddr], count: CInt): Unit = extern

  // Creates a connected AF_UNIX pair of the given socket type; CLOEXEC is always applied. Each fd is
  // then adopted into a uv_pipe_t (a Unix-domain stream) with uv_pipe_open.
  def uv_socketpair(`type`: CInt, protocol: CInt, socketVector: Ptr[CInt], flags0: CInt, flags1: CInt): CInt = extern
  def uv_pipe_open(handle: Ptr[Byte], file: CInt): CInt = extern

  def uv_ip4_addr(ip: CString, port: CInt, addr: Ptr[Byte]): CInt = extern
  def uv_ip6_addr(ip: CString, port: CInt, addr: Ptr[Byte]): CInt = extern
  def uv_ip4_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern
  def uv_ip6_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern

  def uv_strerror_r(err: CInt, buf: CString, buflen: CSize): CString = extern
  def uv_err_name_r(err: CInt, buf: CString, buflen: CSize): CString = extern
  def uv_version(): CUnsignedInt = extern

end LibUVExtern
