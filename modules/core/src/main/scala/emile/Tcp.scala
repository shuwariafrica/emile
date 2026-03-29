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

import scala.scalanative.libc.stdlib.calloc
import scala.scalanative.libc.stdlib.free
import scala.scalanative.libc.stdlib.malloc
import scala.scalanative.posix.sys.socket.sockaddr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.nullable.*

import emile.ipa.SocketAddress
import emile.ipa.fromSockAddr
import emile.ipa.toSockAddr
import emile.unsafe.CallbackStore
import emile.unsafe.LibUV

// scalafix:off; libuv FFI TCP handle

/** TCP handle for network communication.
  *
  * TCP handles are used for TCP servers and clients. They support:
  *   - Server mode: bind, listen, accept connections
  *   - Client mode: connect to remote servers
  *   - Both: read and write data
  *
  * The type parameter `S` tracks the handle's lifecycle state at compile time:
  *   - `Tcp[Open]`: Handle is initialised and ready for operations
  *   - `Tcp[Closed]`: Handle has been closed, operations are prevented
  *
  * ==Server Example==
  * {{{
  * import emile.ipa.*
  *
  * for
  *   server <- Tcp.init(loop)
  *   port <- Port.fromInt(8080).toRight(EmileError.InvalidAddress("", "invalid port"))
  *   _ <- server.bind(SocketAddress.v4(Ipv4Address.Any, port))
  *   _ <- server.listen(128) { status =>
  *     if status >= 0 then
  *       for client <- Tcp.init(loop); _ <- server.accept(client)
  *       yield client.readStart { data => println(s"Received: $data") }
  *   }
  * yield server
  * }}}
  *
  * ==Client Example==
  * {{{
  * import emile.ipa.*
  *
  * for
  *   client <- Tcp.init(loop)
  *   port <- Port.fromInt(8080).toRight(EmileError.InvalidAddress("", "invalid port"))
  *   _ <- client.connect(SocketAddress.v4(Ipv4Address.Loopback, port)) { status =>
  *     if status >= 0 then client.write("Hello!")
  *   }
  * yield client
  * }}}
  *
  * @tparam S The handle state - either `Open` or `Closed`
  * @see [[https://docs.libuv.org/en/stable/tcp.html libuv TCP documentation]]
  */
opaque type Tcp[S <: HandleState] = Ptr[Byte]

object Tcp:
  /** Type alias for an open TCP handle. */
  type OpenTcp = Tcp[Open]

  /** Type alias for a closed TCP handle. */
  type ClosedTcp = Tcp[Closed]

  given [S <: HandleState]: CanEqual[Tcp[S], Tcp[S]] = CanEqual.derived

  /** Provide Handle type class instance for Tcp. */
  given [S <: HandleState]: Handle[Tcp[S]] = Handle.fromPtr[Tcp[S]](_.ptr)

  // Handle type constant for uv_handle_size
  // toLibuvInline provides compile-time elimination when used directly
  private val UV_TCP = HandleType.toLibuvInline(HandleType.Tcp)

  // Request type constants for uv_req_size - use RequestType enum for correctness
  private val UV_CONNECT: CInt = RequestType.Connect.toLibuv // 2
  private val UV_WRITE: CInt = RequestType.Write.toLibuv // 3
  private val UV_SHUTDOWN: CInt = RequestType.Shutdown.toLibuv // 4

  // TCP bind flag constants from libuv
  private val UV_TCP_IPV6ONLY: Int = 1
  private val UV_TCP_REUSEPORT: Int = 2

  /** Initialize a new TCP handle with libuv defaults.
    *
    * This is the most efficient path when no configuration overrides are needed.
    *
    * @param loop The event loop
    * @return Either an error or the initialised TCP handle in Open state
    */
  def init(loop: Loop): Either[EmileError, Tcp[Open]] =
    val size = LibUV.uv_handle_size(UV_TCP)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
      val result = LibUV.uv_tcp_init(loop.ptrUnsafe, handle)
      if result < 0 then
        free(handle)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(handle)
    }

  /** Initialize a new TCP handle with the specified configuration overrides.
    *
    * Only values explicitly set in the config are applied - unset values use libuv's built-in
    * defaults.
    *
    * The configuration settings (noDelay, keepAlive, simultaneousAccepts) are applied immediately
    * after initialization.
    *
    * @param loop The event loop
    * @param config TCP configuration overrides
    * @return Either an error or the initialised TCP handle in Open state
    */
  def init(loop: Loop, config: TcpConfig): Either[EmileError, Tcp[Open]] =
    // Fast path: no handle overrides = use direct init
    if !config.hasHandleOverrides then init(loop)
    else
      val size = LibUV.uv_handle_size(UV_TCP)
      calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { handle =>
        val result = LibUV.uv_tcp_init(loop.ptrUnsafe, handle)
        if result < 0 then
          free(handle)
          Left(EmileError.fromErrorCode(ErrorCode(result)))
        else
          val tcp: Tcp[Open] = handle
          // Apply only specified configuration overrides
          tcp.applyConfig(config) match
            case Left(err) =>
              free(handle)
              Left(err)
            case Right(_) =>
              Right(tcp)
        end if
      }

  /** Internal constructor from raw pointer. */
  private[emile] inline def apply[S <: HandleState](p: Ptr[Byte]): Tcp[S] = p

  // ============================================================================
  // Common extensions (all states)
  // ============================================================================

  extension [S <: HandleState](tcp: Tcp[S])
    /** Get the raw pointer. */
    private[emile] inline def ptr: Ptr[Byte] = tcp

  // ============================================================================
  // Open state operations
  // ============================================================================

  extension (tcp: Tcp[Open])
    /** Bind the TCP handle to an address with libuv default flags.
      *
      * This is typically used for servers to specify the address to listen on.
      *
      * @param address The address to bind to
      * @return Either an error or success
      */
    def bind(address: SocketAddress): Either[EmileError, Unit] =
      Zone:
        val sockaddr = address.toSockAddr
        val result = LibUV.uv_tcp_bind(tcp, sockaddr.asInstanceOf[Ptr[Byte]], 0.toUInt)
        if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
        else Right(())

    /** Bind the TCP handle to an address with the specified configuration flags.
      *
      * Only explicitly set flags are applied - unset values use libuv defaults.
      *   - reusePort: Enable SO_REUSEPORT for load balancing (platform-specific)
      *   - ipv6Only: Disable dual-stack on IPv6 sockets
      *
      * @param address The address to bind to
      * @param config TCP configuration containing bind flags
      * @return Either an error or success
      */
    def bind(address: SocketAddress, config: TcpConfig): Either[EmileError, Unit] =
      // Fast path: no bind overrides = use direct bind
      if !config.hasBindOverrides then bind(address)
      else
        Zone:
          val sockaddr = address.toSockAddr
          var flags: Int = 0
          if config.ipv6Only.contains(true) then flags |= UV_TCP_IPV6ONLY
          if config.reusePort.contains(true) then flags |= UV_TCP_REUSEPORT
          val result = LibUV.uv_tcp_bind(tcp, sockaddr.asInstanceOf[Ptr[Byte]], flags.toUInt)
          if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
          else Right(())

    /** Get the local socket address (IP and port) that this TCP handle is bound to.
      *
      * This is useful after binding to port 0 (ephemeral port) to discover the actual port assigned
      * by the OS.
      *
      * @return Either an error or the bound address
      */
    def getSocketName: Either[EmileError, SocketAddress] =
      Zone:
        // sockaddr_storage is 128 bytes, large enough for any address
        val storage = alloc[Byte](128)
        val namelen = alloc[CInt]()
        !namelen = 128

        val result = LibUV.uv_tcp_getsockname(tcp, storage, namelen)
        if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
        else
          fromSockAddr(storage.asInstanceOf[Ptr[sockaddr]]) match
            case Left(err)         => Left(EmileError.InvalidAddress("unknown", err.message))
            case Right(socketAddr) => Right(socketAddr)

    /** Start listening for incoming connections.
      *
      * The callback is invoked when a new connection arrives. Use `accept` to accept the connection
      * into a new TCP handle.
      *
      * @param backlog Maximum pending connections in the listen queue
      * @param callback Callback invoked on new connections (status: 0 = success)
      * @return Either an error or success
      */
    def listen(backlog: Int)(callback: Int => Unit): Either[EmileError, Unit] =
      // Detach any existing callback
      CallbackStore.detach(tcp)

      CallbackStore.attach(tcp, callback)

      val result = LibUV.uv_listen(tcp, backlog, connectionCallback)
      if result < 0 then
        CallbackStore.detach(tcp)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Accept an incoming connection.
      *
      * This must be called from the listen callback. The client handle must be a newly initialised
      * (not yet connected) TCP handle.
      *
      * @param client The TCP handle to accept the connection into
      * @return Either an error or success
      */
    def accept(client: Tcp[Open]): Either[EmileError, Unit] =
      val result = LibUV.uv_accept(tcp, client.ptr)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Connect to a remote address.
      *
      * The callback is invoked when the connection completes (or fails).
      *
      * @param address The remote address to connect to
      * @param callback Callback invoked when connection completes (status: 0 = success)
      * @return Either an error or success
      */
    def connect(address: SocketAddress)(callback: Int => Unit): Either[EmileError, Unit] =
      Zone:
        val sockaddr = address.toSockAddr
        val reqSize = LibUV.uv_req_size(UV_CONNECT)
        malloc(reqSize).either(EmileError.OutOfMemory).flatMap { req =>
          CallbackStore.attachReq(req, callback)

          val result = LibUV.uv_tcp_connect(req, tcp, sockaddr.asInstanceOf[Ptr[Byte]], connectCallback)
          if result < 0 then
            val _ = CallbackStore.detachReq[Int => Unit](req)
            free(req)
            Left(EmileError.fromErrorCode(ErrorCode(result)))
          else Right(())
        }

    /** Start reading data from the connection.
      *
      * The callback receives data chunks as they arrive. An empty array signals EOF (remote closed
      * the connection).
      *
      * @param callback Callback invoked with received data or error
      * @return Either an error or success
      */
    def readStart(callback: Either[EmileError, Array[Byte]] => Unit): Either[EmileError, Unit] =
      // Detach any existing read callback
      CallbackStore.detach(tcp)

      CallbackStore.attach(tcp, callback)

      val result = LibUV.uv_read_start(tcp, allocCallback, readCallback)
      if result < 0 then
        CallbackStore.detach(tcp)
        Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Stop reading data.
      *
      * @return Either an error or success
      */
    def readStop: Either[EmileError, Unit] =
      val result = LibUV.uv_read_stop(tcp)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else
        CallbackStore.detach(tcp)
        Right(())

    /** Write data to the connection.
      *
      * @param data The data to write
      * @param callback Callback when write completes
      * @return Either an error or success
      */
    def write(data: Array[Byte])(callback: Int => Unit): Either[EmileError, Unit] =
      if data.isEmpty then Right(())
      else
        val reqSize = LibUV.uv_req_size(UV_WRITE)
        malloc(reqSize).either(EmileError.OutOfMemory).flatMap { req =>
          malloc(data.length.toCSize).option match
            case None =>
              free(req)
              Left(EmileError.OutOfMemory)
            case Some(dataPtr) =>
              // Copy data to native memory
              var i = 0
              while i < data.length do
                !(dataPtr + i) = data(i)
                i += 1

              // uv_write copies uv_buf_t contents, so stack allocation is safe; dataPtr must live until callback
              val buf = stackalloc[LibUV.Buffer](1)
              buf._1 = dataPtr
              buf._2 = data.length.toCSize

              val writeData = (callback, dataPtr)
              CallbackStore.attachReq(req, writeData)

              val result = LibUV.uv_write(req, tcp, buf, 1.toUInt, writeCallback)
              if result < 0 then
                val _ = CallbackStore.detachReq[(Int => Unit, Ptr[Byte])](req)
                free(dataPtr)
                free(req)
                Left(EmileError.fromErrorCode(ErrorCode(result)))
              else Right(())
        }

    /** Write data to the connection, discarding the write status. */
    def write(data: Array[Byte]): Either[EmileError, Unit] =
      write(data)(_ => ())

    /** Write a string to the connection (UTF-8 encoded).
      *
      * @param str The string to write
      * @param callback Callback when write completes
      * @return Either an error or success
      */
    def writeString(str: String)(callback: Int => Unit): Either[EmileError, Unit] =
      write(str.getBytes("UTF-8"))(callback)

    /** Write a string to the connection (UTF-8 encoded), discarding the write status. */
    def writeString(str: String): Either[EmileError, Unit] =
      write(str.getBytes("UTF-8"))(_ => ())

    /** Shutdown the write side of the connection.
      *
      * This sends a FIN to signal we're done writing. Reading can still continue.
      *
      * @param callback Callback when shutdown completes
      * @return Either an error or success
      */
    def shutdown(callback: Int => Unit): Either[EmileError, Unit] =
      val reqSize = LibUV.uv_req_size(UV_SHUTDOWN)
      malloc(reqSize).either(EmileError.OutOfMemory).flatMap { req =>
        CallbackStore.attachReq(req, callback)

        val result = LibUV.uv_shutdown(req, tcp, shutdownCallback)
        if result < 0 then
          val _ = CallbackStore.detachReq[Int => Unit](req)
          free(req)
          Left(EmileError.fromErrorCode(ErrorCode(result)))
        else Right(())
      }
    end shutdown

    /** Shutdown the write side of the connection, discarding status. */
    def shutdown: Either[EmileError, Unit] =
      shutdown(_ => ())

    /** Enable/disable TCP_NODELAY (disable Nagle's algorithm).
      *
      * @param enable true to disable Nagle's algorithm
      * @return Either an error or success
      */
    def setNoDelay(enable: Boolean): Either[EmileError, Unit] =
      val result = LibUV.uv_tcp_nodelay(tcp, if enable then 1 else 0)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Enable/disable TCP keepalive.
      *
      * @param enable true to enable keepalive
      * @param delay Initial delay in seconds
      * @return Either an error or success
      */
    def setKeepAlive(enable: Boolean, delay: Int): Either[EmileError, Unit] =
      val result = LibUV.uv_tcp_keepalive(tcp, if enable then 1 else 0, delay.toUInt)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Disable TCP keepalive. */
    def disableKeepAlive: Either[EmileError, Unit] =
      setKeepAlive(false, 0)

    /** Enable/disable simultaneous accepts.
      *
      * When enabled, the OS queues multiple accept requests which improves connection acceptance
      * rate but may lead to uneven load distribution in multi-process setups.
      *
      * @param enable true to enable simultaneous accepts (default in libuv)
      * @return Either an error or success
      */
    def setSimultaneousAccepts(enable: Boolean): Either[EmileError, Unit] =
      val result = LibUV.uv_tcp_simultaneous_accepts(tcp, if enable then 1 else 0)
      if result < 0 then Left(EmileError.fromErrorCode(ErrorCode(result)))
      else Right(())

    /** Apply TcpConfig overrides to this handle.
      *
      * Only explicitly set values in the config are applied - unset values leave libuv defaults
      * unchanged.
      *
      * Applies:
      *   - noDelay: TCP_NODELAY setting (if specified)
      *   - keepAlive: TCP keep-alive settings (if specified)
      *   - simultaneousAccepts: accept queue behaviour (if specified)
      *
      * Note: reusePort and ipv6Only are applied during bind, not here.
      *
      * @param config The TCP configuration overrides to apply
      * @return Either an error or success
      */
    def applyConfig(config: TcpConfig): Either[EmileError, Unit] =
      for
        _ <- config.noDelay.fold(Right(()): Either[EmileError, Unit])(setNoDelay)
        _ <- config.keepAlive match
               case Some(TcpKeepAlive.Disabled)         => disableKeepAlive
               case Some(TcpKeepAlive.Simple(delay))    => setKeepAlive(true, delay)
               case Some(TcpKeepAlive.Full(idle, _, _)) =>
                 // Note: uv_tcp_keepalive only supports simple delay
                 // Full options would need uv_tcp_keepalive_ex (libuv 1.52+)
                 setKeepAlive(true, idle)
               case None => Right(())
        _ <- config.simultaneousAccepts.fold(Right(()): Either[EmileError, Unit])(setSimultaneousAccepts)
      yield ()

    /** Check if the stream is readable. */
    def isReadable: Boolean =
      LibUV.uv_is_readable(tcp) != 0

    /** Check if the stream is writable. */
    def isWritable: Boolean =
      LibUV.uv_is_writable(tcp) != 0

    /** Close the TCP handle asynchronously with a callback.
      *
      * The callback is invoked when the handle has been fully closed. Note: The handle transitions
      * to Closed state and cannot return a witness.
      *
      * @param callback Function called when close completes
      */
    def closeAsync(callback: Either[EmileError, Unit] => Unit): Unit =
      Handle.closeAsyncWithDetach(tcp, callback)
  end extension

  // ============================================================================
  // Callbacks
  // ============================================================================

  /** Connection callback for listen. */
  private val connectionCallback: LibUV.ConnectionCB = (server: Ptr[Byte], status: CInt) =>
    CallbackStore.get[Int => Unit](server).foreach { callback =>
      callback(status)
    }

  /** Connect callback. */
  private val connectCallback: LibUV.ConnectCB = (req: Ptr[Byte], status: CInt) =>
    CallbackStore.detachReq[Int => Unit](req).foreach { callback =>
      callback(status)
    }
    free(req)

  /** Allocation callback for read. */
  @annotation.nowarn("msg=unused explicit parameter")
  private val allocCallback: LibUV.AllocCB = (handle: Ptr[Byte], suggestedSize: CSize, buf: Ptr[LibUV.Buffer]) =>
    val ptr = malloc(suggestedSize)
    buf._1 = ptr
    buf._2 = if ptr != null then suggestedSize else 0.toCSize

  /** Read callback. */
  private val readCallback: LibUV.ReadCB = (stream: Ptr[Byte], nread: CSSize, buf: Ptr[LibUV.Buffer]) =>
    CallbackStore.get[Either[EmileError, Array[Byte]] => Unit](stream).foreach { callback =>
      if nread > 0 then
        // Got data
        val data = new Array[Byte](nread.toInt)
        val basePtr = buf._1
        var i = 0
        while i < nread.toInt do
          data(i) = !(basePtr + i)
          i += 1
        callback(Right(data))
      else if nread == 0 then
        // EAGAIN - no data but not an error
        ()
      else if nread == ErrorCode.Eof.value then
        // EOF - remote closed
        callback(Right(Array.empty[Byte]))
      else
        // Error
        callback(Left(EmileError.fromErrorCode(ErrorCode(nread.toInt))))
    }
    // Always free the buffer
    if buf._1 != null then free(buf._1)

  /** Write callback. */
  private val writeCallback: LibUV.WriteCB = (req: Ptr[Byte], status: CInt) =>
    CallbackStore.detachReq[(Int => Unit, Ptr[Byte])](req).foreach { case (callback, dataPtr) =>
      free(dataPtr)
      callback(status)
    }
    free(req)

  /** Shutdown callback. */
  private val shutdownCallback: LibUV.ShutdownCB = (req: Ptr[Byte], status: CInt) =>
    CallbackStore.detachReq[Int => Unit](req).foreach { callback =>
      callback(status)
    }
    free(req)

end Tcp
