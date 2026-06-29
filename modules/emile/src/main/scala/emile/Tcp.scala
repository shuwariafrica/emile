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

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue
import com.comcast.ip4s.Host
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibuvPoller
import emile.unsafe.Routing
import emile.unsafe.SockAddr

/** Entry points for TCP bind and connect. Each operation runs on the worker that acquires the
  * resource; the resulting [[TcpServer]] / [[TcpSocket]] carries that worker's loop.
  */
object Tcp:

  /** Bind a listening server on `address` with the default [[TcpOptions]]. */
  def bind(address: SocketAddress[IpAddress]): EmResource[EmileError.Bind, TcpServer] =
    bind(address, TcpOptions.default)

  /** Bind a listening server on `address` with `options`. Binding and listening complete during
    * acquire, so every failure surfaces here rather than later on the connection stream.
    */
  def bind(address: SocketAddress[IpAddress], options: TcpOptions): EmResource[EmileError.Bind, TcpServer] =
    Resource.make[EffIO.Of[EmileError.Bind], TcpServer](bindAcquire(address, options))(server => EffIO.liftF(TcpServer.release(server)))

  /** Connect to `address` with the default [[TcpOptions]]. */
  def connect(address: SocketAddress[IpAddress]): EmResource[EmileError.Connect, TcpSocket] =
    connect(address, TcpOptions.default)

  /** Connect to `address` with `options`. The connect is cancelable: a `timeout` or cancellation
    * aborts the in-flight `uv_tcp_connect` and frees the handle.
    */
  def connect(address: SocketAddress[IpAddress], options: TcpOptions): EmResource[EmileError.Connect, TcpSocket] =
    Resource
      .makeFull[EffIO.Of[EmileError.Connect], TcpSocket](poll => poll(connectRaw(address)))(socket => EffIO.liftF(Socket.release(socket)))
      .evalTap(applyPostConnect(_, options))

  /** Connect by hostname with the default [[TcpOptions]]. */
  def connect(host: Host, port: Port): EmResource[EmileError.HostConnect, TcpSocket] =
    connect(host, port, TcpOptions.default)

  /** Connect by hostname with `options`. Resolves the host through [[Dns]] then attempts the
    * addresses serially in resolver order; the first success wins. Resolver failure surfaces as
    * [[EmileError.Dns]], connect failure as [[EmileError.Connect]] - both flow through their common
    * parent [[EmileError.HostConnect]].
    */
  def connect(host: Host, port: Port, options: TcpOptions): EmResource[EmileError.HostConnect, TcpSocket] =
    Resource
      .makeFull[EffIO.Of[EmileError.HostConnect], TcpSocket](poll => poll(hostConnectAcquire(host, port)))(socket =>
        EffIO.liftF(Socket.release(socket))
      )
      .evalTap(applyPostConnect(_, options))

  private def bindAcquire(address: SocketAddress[IpAddress], options: TcpOptions): EmIO[EmileError.Bind, TcpServer] =
    EffIO.attempt(
      for
        poller <- LibuvPollingSystem.currentPoller
        queue <- UnboundedQueue[IO, Either[EmileError.Io, Unit]]
        result <- Routing.onOwner(poller)(bindInstall(poller, address, options, queue))
        server <- IO.fromEither(result)
      yield server,
      EmileError.Bind.Unexpected(_)
    )

  // FFI: handle / sockaddr calloc null-checks, uv_close cleanup paths for half-built resources.
  // scalafix:off DisableSyntax

  private def bindInstall(
    poller: LibuvPoller,
    address: SocketAddress[IpAddress],
    options: TcpOptions,
    queue: UnboundedQueue[IO, Either[EmileError.Io, Unit]]
  ): Either[EmileError.Bind, TcpServer] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_TCP))
    if handle == null then throw new OutOfMemoryError("emile: uv_tcp_t allocation failed")
    val initRc = LibUV.uv_tcp_init(poller.loop, handle)
    if initRc != 0 then
      stdlib.free(handle)
      Left(BindMapping.fromCode(initRc))
    else
      configurePreBind(handle, options) match
        case Left(error) =>
          LibUV.uv_close(handle, freeHandleCb)
          Left(error)
        case Right(()) => bindAndListen(poller, handle, address, options, queue)
  end bindInstall

  private def bindAndListen(
    poller: LibuvPoller,
    handle: Ptr[Byte],
    address: SocketAddress[IpAddress],
    options: TcpOptions,
    queue: UnboundedQueue[IO, Either[EmileError.Io, Unit]]
  ): Either[EmileError.Bind, TcpServer] =
    val sockaddr = stdlib.calloc(1.toCSize, SockAddr.storageSize.toCSize)
    if sockaddr == null then
      LibUV.uv_close(handle, freeHandleCb)
      throw new OutOfMemoryError("emile: sockaddr allocation failed")
    SockAddr.write(address, sockaddr)
    val bindRc = LibUV.uv_tcp_bind(handle, sockaddr, bindFlags(options))
    stdlib.free(sockaddr)
    if bindRc != 0 then
      LibUV.uv_close(handle, freeHandleCb)
      Left(BindMapping.fromCode(bindRc))
    else
      Socket.localAddressOf(handle) match
        case Left(rc) =>
          LibUV.uv_close(handle, freeHandleCb)
          Left(
            if rc == 0 then EmileError.Bind.InvalidAddress("unsupported address family")
            else BindMapping.fromCode(rc)
          )
        case Right(local) =>
          // TcpServer.construct stores the server in the handle's `data` slot - which the
          // connection_cb later recovers - so it has to happen before uv_listen activates and
          // a connection_cb could fire on an unstored handle.
          val server = TcpServer.construct(handle, poller, local, options, queue)
          val listenRc = LibUV.uv_listen(handle, options.listenBacklog, TcpServer.connectionCb)
          if listenRc != 0 then
            // construct anchored the server in the data slot; clear it before uv_close or it leaks.
            CallbackBridge.clear(poller, handle)
            LibUV.uv_close(handle, freeHandleCb)
            Left(BindMapping.fromCode(listenRc))
          else Right(server)
    end if
  end bindAndListen

  private def configurePreBind(handle: Ptr[Byte], options: TcpOptions): Either[EmileError.Bind, Unit] =
    if options.simultaneousAccepts then Right(())
    else
      val rc = LibUV.uv_tcp_simultaneous_accepts(handle, 0)
      if rc != 0 then Left(BindMapping.fromCode(rc)) else Right(())

  private def bindFlags(options: TcpOptions): CUnsignedInt =
    var flags = 0
    if options.reusePort then flags = flags | LibUV.UV_TCP_REUSEPORT
    if options.ipv6Only then flags = flags | LibUV.UV_TCP_IPV6ONLY
    flags.toUInt

  private def connectRaw(address: SocketAddress[IpAddress]): EmIO[EmileError.Connect, TcpSocket] =
    EffIO.attempt(
      for
        poller <- LibuvPollingSystem.currentPoller
        socket <- performConnect(poller, address)
      yield socket,
      EmileError.Connect.Unexpected(_)
    )

  private def performConnect(poller: LibuvPoller, address: SocketAddress[IpAddress]): IO[TcpSocket] =
    IO.async[TcpSocket] { cb =>
      Routing.onOwner(poller):
        val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_TCP))
        if handle == null then throw new OutOfMemoryError("emile: uv_tcp_t allocation failed")
        val initRc = LibUV.uv_tcp_init(poller.loop, handle)
        if initRc != 0 then
          stdlib.free(handle)
          cb(Left(ConnectMapping.fromCode(initRc)))
          None
        else startConnect(poller, handle, address, cb)
    }

  private def startConnect(
    poller: LibuvPoller,
    handle: Ptr[Byte],
    address: SocketAddress[IpAddress],
    cb: Either[Throwable, TcpSocket] => Unit
  ): Option[IO[Unit]] =
    val req = allocConnectReq()
    val sockaddr = stdlib.calloc(1.toCSize, SockAddr.storageSize.toCSize)
    if sockaddr == null then
      stdlib.free(req)
      LibUV.uv_close(handle, freeHandleCb)
      throw new OutOfMemoryError("emile: sockaddr allocation failed")
    SockAddr.write(address, sockaddr)
    CallbackBridge.storeReq(poller, req, connectDeliver(cb, poller, handle))
    val connectRc = LibUV.uv_tcp_connect(req, handle, sockaddr, connectCb)
    stdlib.free(sockaddr)
    if connectRc != 0 then
      CallbackBridge.releaseReq(poller, req)
      stdlib.free(req)
      LibUV.uv_close(handle, freeHandleCb)
      cb(Left(ConnectMapping.fromCode(connectRc)))
      None
    else
      // Cancellation finaliser: uv_close the in-flight handle, which makes connectCb fire with
      // UV_ECANCELED (its connectDeliver, seeing uv_is_closing, frees only the req) and the close
      // callback frees the handle.
      Some(Routing.onOwner(poller)(abortConnect(handle)))
  end startConnect

  private def abortConnect(handle: Ptr[Byte]): Unit =
    if LibUV.uv_is_closing(handle) == 0 then LibUV.uv_close(handle, freeHandleCb)

  private def connectDeliver(
    cb: Either[Throwable, TcpSocket] => Unit,
    poller: LibuvPoller,
    handle: Ptr[Byte]
  ): (Int, Ptr[Byte]) => Unit =
    (status, req) =>
      CallbackBridge.releaseReq(poller, req)
      stdlib.free(req)
      if LibUV.uv_is_closing(handle) != 0 then () // cancelled: the finaliser uv_close'd the handle; cb is dead
      else if status < 0 then
        LibUV.uv_close(handle, freeHandleCb)
        cb(Left(ConnectMapping.fromCode(status)))
      else
        Socket.localAddressOf(handle) match
          case Left(rc) =>
            LibUV.uv_close(handle, freeHandleCb)
            cb(Left(toConnectError(rc)))
          case Right(local) =>
            Socket.peerAddressOf(handle) match
              case Left(rc) =>
                LibUV.uv_close(handle, freeHandleCb)
                cb(Left(toConnectError(rc)))
              case Right(peer) =>
                cb(Right(Socket.construct(handle, poller, local, peer)))
      end if

  private val connectCb: LibUV.ConnectCB = (req: Ptr[Byte], status: CInt) =>
    CallbackBridge.loadReq[(Int, Ptr[Byte]) => Unit](req).apply(status, req)

  private def toConnectError(rc: Int): EmileError.Connect =
    if rc == 0 then EmileError.Connect.Unexpected(new IllegalStateException("emile: unsupported TCP address family"))
    else ConnectMapping.fromCode(rc)

  private def applyPostConnect(socket: TcpSocket, options: TcpOptions): EmIO[EmileError.Connect, Unit] =
    Socket.applyOptions(socket, options).mapError(EmileError.Connect.Unexpected(_))

  private def hostConnectAcquire(host: Host, port: Port): EmIO[EmileError.HostConnect, TcpSocket] =
    Dns.resolve(host, port).flatMap(addresses => tryAddresses(addresses.toList, Nil))

  private def tryAddresses(
    addresses: List[SocketAddress[IpAddress]],
    failures: List[EmileError.Connect]
  ): EmIO[EmileError.HostConnect, TcpSocket] =
    addresses match
      case Nil =>
        // failures accumulate newest-first; restore resolver order. A single failure surfaces
        // directly; multiple are aggregated so no per-address diagnostic is discarded.
        val error: EmileError.Connect = NonEmptyList.fromList(failures.reverse) match
          case Some(NonEmptyList(only, Nil)) => only
          case Some(nel) => EmileError.Connect.AllAddressesFailed(nel)
          case None => EmileError.Connect.Unexpected(new IllegalStateException("emile: empty DNS result"))
        EffIO.fail(error)
      case head :: rest => connectRaw(head).catchAll(err => tryAddresses(rest, err :: failures))

  private def allocConnectReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_CONNECT))
    if req == null then throw new OutOfMemoryError("emile: uv_connect_t allocation failed")
    else req

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end Tcp
