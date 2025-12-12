/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.unsafe

import scala.scalanative.unsafe.*

/**
 * Low-level libuv FFI bindings.
 *
 * INTERNAL: These are direct C function bindings and should not be used
 * directly by library users. Use the safe wrappers in the main package.
 */
@link("uv")
@extern
private[emile] object LibUV:
  // ==========================================================================
  // Type aliases for libuv types
  // ==========================================================================

  /** libuv buffer structure: CStruct2[Ptr[Byte], CSize] */
  type Buffer = CStruct2[Ptr[Byte], CSize]

  /** Callback types */
  type CloseCB = CFuncPtr1[Ptr[Byte], Unit]
  type AllocCB = CFuncPtr3[Ptr[Byte], CSize, Ptr[Buffer], Unit]
  type ReadCB = CFuncPtr3[Ptr[Byte], CSSize, Ptr[Buffer], Unit]
  type WriteCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type ConnectionCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type ConnectCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type ShutdownCB = CFuncPtr2[Ptr[Byte], CInt, Unit]
  type TimerCB = CFuncPtr1[Ptr[Byte], Unit]
  type AsyncCB = CFuncPtr1[Ptr[Byte], Unit]
  type PollCB = CFuncPtr3[Ptr[Byte], CInt, CInt, Unit]

  // ==========================================================================
  // Loop functions
  // ==========================================================================

  /** Get the default event loop. */
  def uv_default_loop(): Ptr[Byte] = extern

  /** Get size of uv_loop_t structure for allocation. */
  def uv_loop_size(): CSize = extern

  /** Initialise a loop. */
  def uv_loop_init(loop: Ptr[Byte]): CInt = extern

  /** Walk all handles in the loop. Callback receives each handle and the arg. */
  type WalkCB = CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]
  def uv_walk(loop: Ptr[Byte], walkCb: WalkCB, arg: Ptr[Byte]): Unit = extern

  /** Close and free resources for a loop. */
  def uv_loop_close(loop: Ptr[Byte]): CInt = extern

  /** Run the event loop. */
  def uv_run(loop: Ptr[Byte], mode: CInt): CInt = extern

  /** Stop the event loop. */
  def uv_stop(loop: Ptr[Byte]): Unit = extern

  /** Check if loop has active handles or requests. */
  def uv_loop_alive(loop: Ptr[Byte]): CInt = extern

  /** Get current cached timestamp from loop. */
  def uv_now(loop: Ptr[Byte]): CUnsignedLongLong = extern

  /** Update the cached timestamp. */
  def uv_update_time(loop: Ptr[Byte]): Unit = extern

  /** Get the loop's backend file descriptor (for embedding). */
  def uv_backend_fd(loop: Ptr[Byte]): CInt = extern

  /** Get the poll timeout for the loop. */
  def uv_backend_timeout(loop: Ptr[Byte]): CInt = extern

  /** Configure loop options. Second parameter is option-specific. */
  def uv_loop_configure(loop: Ptr[Byte], option: CInt, arg: CInt): CInt = extern

  /** Get idle time metrics (requires UV_METRICS_IDLE_TIME to be enabled). */
  def uv_metrics_idle_time(loop: Ptr[Byte]): CUnsignedLongLong = extern

  // ==========================================================================
  // Handle functions (common to all handle types)
  // ==========================================================================

  /** Get size of handle type for allocation. */
  def uv_handle_size(handleType: CInt): CSize = extern

  /** Check if handle is active. */
  def uv_is_active(handle: Ptr[Byte]): CInt = extern

  /** Check if handle is closing or closed. */
  def uv_is_closing(handle: Ptr[Byte]): CInt = extern

  /** Close a handle. */
  def uv_close(handle: Ptr[Byte], closeCb: CloseCB): Unit = extern

  /** Reference a handle. */
  def uv_ref(handle: Ptr[Byte]): Unit = extern

  /** Unreference a handle. */
  def uv_unref(handle: Ptr[Byte]): Unit = extern

  /** Check if handle is referenced. */
  def uv_has_ref(handle: Ptr[Byte]): CInt = extern

  /** Get the loop from a handle. */
  def uv_handle_get_loop(handle: Ptr[Byte]): Ptr[Byte] = extern

  /** Get handle type. */
  def uv_handle_get_type(handle: Ptr[Byte]): CInt = extern

  /** Get user data from handle. */
  def uv_handle_get_data(handle: Ptr[Byte]): Ptr[Byte] = extern

  /** Set user data on handle. */
  def uv_handle_set_data(handle: Ptr[Byte], data: Ptr[Byte]): Unit = extern

  // ==========================================================================
  // Request functions
  // ==========================================================================

  /** Get size of request type for allocation. */
  def uv_req_size(reqType: CInt): CSize = extern

  /** Get user data from request. */
  def uv_req_get_data(req: Ptr[Byte]): Ptr[Byte] = extern

  /** Set user data on request. */
  def uv_req_set_data(req: Ptr[Byte], data: Ptr[Byte]): Unit = extern

  // ==========================================================================
  // TCP functions
  // ==========================================================================

  /** Initialise a TCP handle. */
  def uv_tcp_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern

  /** Initialise TCP handle with specific flags. */
  def uv_tcp_init_ex(loop: Ptr[Byte], handle: Ptr[Byte], flags: CUnsignedInt): CInt = extern

  /** Bind TCP handle to address. */
  def uv_tcp_bind(handle: Ptr[Byte], addr: Ptr[Byte], flags: CUnsignedInt): CInt = extern

  /** Get local socket address. */
  def uv_tcp_getsockname(handle: Ptr[Byte], name: Ptr[Byte], namelen: Ptr[CInt]): CInt = extern

  /** Get peer socket address. */
  def uv_tcp_getpeername(handle: Ptr[Byte], name: Ptr[Byte], namelen: Ptr[CInt]): CInt = extern

  /** Enable/disable TCP_NODELAY. */
  def uv_tcp_nodelay(handle: Ptr[Byte], enable: CInt): CInt = extern

  /** Enable/disable TCP keep-alive. */
  def uv_tcp_keepalive(handle: Ptr[Byte], enable: CInt, delay: CUnsignedInt): CInt = extern

  /** Enable/disable simultaneous accepts. */
  def uv_tcp_simultaneous_accepts(handle: Ptr[Byte], enable: CInt): CInt = extern

  // ==========================================================================
  // Stream functions (TCP, Pipe, TTY inherit from stream)
  // ==========================================================================

  /** Start listening for incoming connections. */
  def uv_listen(stream: Ptr[Byte], backlog: CInt, cb: ConnectionCB): CInt = extern

  /** Accept a connection. */
  def uv_accept(server: Ptr[Byte], client: Ptr[Byte]): CInt = extern

  /** Start reading from stream. */
  def uv_read_start(stream: Ptr[Byte], allocCb: AllocCB, readCb: ReadCB): CInt = extern

  /** Stop reading from stream. */
  def uv_read_stop(stream: Ptr[Byte]): CInt = extern

  /** Write data to stream. */
  def uv_write(
      req: Ptr[Byte],
      handle: Ptr[Byte],
      bufs: Ptr[Buffer],
      nbufs: CUnsignedInt,
      cb: WriteCB
  ): CInt = extern

  /** Try to write data synchronously. */
  def uv_try_write(handle: Ptr[Byte], bufs: Ptr[Buffer], nbufs: CUnsignedInt): CInt = extern

  /** Shutdown write side. */
  def uv_shutdown(req: Ptr[Byte], handle: Ptr[Byte], cb: ShutdownCB): CInt = extern

  /** Check if stream is readable. */
  def uv_is_readable(handle: Ptr[Byte]): CInt = extern

  /** Check if stream is writable. */
  def uv_is_writable(handle: Ptr[Byte]): CInt = extern

  /** Get write queue size. */
  def uv_stream_get_write_queue_size(stream: Ptr[Byte]): CSize = extern

  // ==========================================================================
  // Connect function
  // ==========================================================================

  /** Connect to remote address. */
  def uv_tcp_connect(req: Ptr[Byte], handle: Ptr[Byte], addr: Ptr[Byte], cb: ConnectCB): CInt = extern

  // ==========================================================================
  // Timer functions
  // ==========================================================================

  /** Initialise a timer handle. */
  def uv_timer_init(loop: Ptr[Byte], handle: Ptr[Byte]): CInt = extern

  /** Start the timer. */
  def uv_timer_start(handle: Ptr[Byte], cb: TimerCB, timeout: CUnsignedLongLong, repeat: CUnsignedLongLong): CInt = extern

  /** Stop the timer. */
  def uv_timer_stop(handle: Ptr[Byte]): CInt = extern

  /** Restart the timer. */
  def uv_timer_again(handle: Ptr[Byte]): CInt = extern

  /** Set repeat interval. */
  def uv_timer_set_repeat(handle: Ptr[Byte], repeat: CUnsignedLongLong): Unit = extern

  /** Get repeat interval. */
  def uv_timer_get_repeat(handle: Ptr[Byte]): CUnsignedLongLong = extern

  /** Get time until timer fires. */
  def uv_timer_get_due_in(handle: Ptr[Byte]): CUnsignedLongLong = extern

  // ==========================================================================
  // Async functions
  // ==========================================================================

  /** Initialise an async handle. */
  def uv_async_init(loop: Ptr[Byte], handle: Ptr[Byte], cb: AsyncCB): CInt = extern

  /** Send async signal (thread-safe). */
  def uv_async_send(handle: Ptr[Byte]): CInt = extern

  // ==========================================================================
  // Poll functions
  // ==========================================================================

  /** Initialise a poll handle for file descriptor. */
  def uv_poll_init(loop: Ptr[Byte], handle: Ptr[Byte], fd: CInt): CInt = extern

  /** Initialise a poll handle for socket. */
  def uv_poll_init_socket(loop: Ptr[Byte], handle: Ptr[Byte], socket: Ptr[Byte]): CInt = extern

  /** Start polling for events. */
  def uv_poll_start(handle: Ptr[Byte], events: CInt, cb: PollCB): CInt = extern

  /** Stop polling. */
  def uv_poll_stop(handle: Ptr[Byte]): CInt = extern

  // ==========================================================================
  // Error functions
  // ==========================================================================

  /** Get error name string. */
  def uv_err_name(err: CInt): CString = extern

  /** Get error description string. */
  def uv_strerror(err: CInt): CString = extern

  // ==========================================================================
  // Address functions
  // ==========================================================================

  /** Convert IPv4 string to sockaddr_in. */
  def uv_ip4_addr(ip: CString, port: CInt, addr: Ptr[Byte]): CInt = extern

  /** Convert IPv6 string to sockaddr_in6. */
  def uv_ip6_addr(ip: CString, port: CInt, addr: Ptr[Byte]): CInt = extern

  /** Convert sockaddr_in to IPv4 string. */
  def uv_ip4_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern

  /** Convert sockaddr_in6 to IPv6 string. */
  def uv_ip6_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern

  /** Convert sockaddr to string. */
  def uv_ip_name(src: Ptr[Byte], dst: CString, size: CSize): CInt = extern
end LibUV
