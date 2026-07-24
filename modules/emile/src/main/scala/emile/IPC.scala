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

import java.nio.charset.StandardCharsets
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.sys.socket as posixSocket
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.OpOutcome
import emile.unsafe.Routing

/** Entry points for local inter-process stream sockets - Unix-domain sockets on Unix, named pipes
  * on Windows ([[SocketKind.IPC]]). Bind a listener or connect a client over an [[IPCAddress]] (a
  * filesystem path, the Linux abstract namespace, or autobind); each operation runs on the worker
  * that acquires the resource, and the resulting [[IPCServer]] / [[IPCSocket]] carries that
  * worker's loop. The byte-stream surface is the shared one on [[Socket$ Socket]].
  */
object IPC:

  // sun_path holds a name of at most 108 bytes on Linux; libuv is asked (UV_PIPE_NO_TRUNCATE) to reject
  // a longer name rather than truncate it to a different socket, and emile rejects it first with a typed
  // error. A validated name is at most this many bytes, so the copy buffer below comfortably holds it.
  private inline val SunPathMax = 108
  private inline val NameMax = 256

  /** Bind a listening server on `address` with the default [[IPCOptions]]. */
  def bind(address: IPCAddress): EmResource[EmileError.Bind, IPCServer] =
    bind(address, IPCOptions.default)

  /** Bind a listening server on `address` with `options`. Binding, any socket-file mode, and
    * listening all complete during acquire, so every failure surfaces here rather than later on the
    * accept stream, and a requested mode is applied before the server ever accepts. A
    * filesystem-path server removes its socket file on release (libuv unlinks it on close).
    */
  def bind(address: IPCAddress, options: IPCOptions): EmResource[EmileError.Bind, IPCServer] =
    Resource.make[EffIO.Of[EmileError.Bind], IPCServer](bindAcquire(address, options))(server => EffIO.liftF(StreamServer.release(server)))

  /** Connect to the server at `address`. The connect is cancelable: a `timeout` or cancellation
    * aborts the in-flight `uv_pipe_connect2` and frees the handle. [[IPCAddress.Autobind]] is
    * bind-only and is rejected here.
    */
  def connect(address: IPCAddress): EmResource[EmileError.Connect, IPCSocket] =
    Resource.makeFull[EffIO.Of[EmileError.Connect], IPCSocket](poll => poll(connectRaw(address)))(socket =>
      EffIO.liftF(Socket.release(socket))
    )

  /** A connected pair of local stream sockets (`socketpair`) - a full-duplex in-process channel
    * with no filesystem entry, for handing one end to a subprocess or splitting work across fibres.
    * Both ends are on the acquiring worker's loop, and both are closed on release.
    */
  def pair: EmResource[EmileError.IO, (IPCSocket, IPCSocket)] =
    Resource.make[EffIO.Of[EmileError.IO], (IPCSocket, IPCSocket)](acquirePair)(releasePair)

  // Shared (not built per bind): an IPC server carries no per-socket options, so finish is a no-op.
  private val ipcAcceptor: StreamServer.Acceptor =
    new StreamServer.Acceptor(
      handleType = LibUV.UV_NAMED_PIPE,
      initClient = (loop, client) => LibUV.uv_pipe_init(loop, client, 0),
      captureAddresses = captureIpcAddresses,
      finish = _ => EffIO.succeed(())
    )

  private def bindAcquire(address: IPCAddress, options: IPCOptions): EmIO[EmileError.Bind, IPCServer] =
    validateBind(address, options) match
      case Some(detail) => EffIO.fail(EmileError.Bind.InvalidAddress(detail))
      case None =>
        EffIO.attempt(
          for
            poller <- LibUVPollingSystem.currentPoller
            queue <- UnboundedQueue[IO, Either[EmileError.IO, Unit]]
            result <- Routing.onOwner(poller)(bindInstall(poller, address, options, queue))
            server <- IO.fromEither(result)
          yield server,
          EmileError.Bind.Unexpected(_)
        )

  private def connectRaw(address: IPCAddress): EmIO[EmileError.Connect, IPCSocket] =
    validateConnect(address) match
      case Some(detail) => EffIO.fail(EmileError.Connect.InvalidAddress(detail))
      case None =>
        EffIO.attempt(
          for
            poller <- LibUVPollingSystem.currentPoller
            socket <- performConnect(poller, address)
          yield socket,
          EmileError.Connect.Unexpected(_)
        )

  // The wire-form byte length writeName will produce, checked against SunPathMax before bind/connect so a
  // name too long for sun_path is rejected rather than truncated to a different socket.
  private def wireNameLength(address: IPCAddress): Int = address match
    case IPCAddress.Path(value) => value.getBytes(StandardCharsets.UTF_8).length
    case IPCAddress.Abstract(name) => 1 + name.getBytes(StandardCharsets.UTF_8).length
    case IPCAddress.Autobind => 0

  private def isFilesystemPath(address: IPCAddress): Boolean = address match
    case IPCAddress.Path(value) => value.nonEmpty
    case _ => false

  private val nameTooLong: String = s"an IPC name must be at most $SunPathMax bytes, the sun_path limit"

  private def validateBind(address: IPCAddress, options: IPCOptions): Option[String] =
    // An empty path encodes to a zero-length name, which on Linux is autobind - a silent surprise rather
    // than the requested bind; IPCAddress.Autobind is the explicit way.
    if address == IPCAddress.Path("") then Some("an IPC bind path must be non-empty; use IPCAddress.Autobind for an unnamed listener")
    else if wireNameLength(address) > SunPathMax then Some(nameTooLong)
    else if options.mode.isDefined && !isFilesystemPath(address) then
      Some("a socket mode applies only to a filesystem-path server; an abstract or autobind socket has no file to chmod")
    else None

  private def validateConnect(address: IPCAddress): Option[String] =
    if address == IPCAddress.Autobind then Some("IPCAddress.Autobind is bind-only; use IPC.bind for an unnamed listener")
    else if address == IPCAddress.Path("") then Some("an IPC connect path must be non-empty")
    else if wireNameLength(address) > SunPathMax then Some(nameTooLong)
    else None

  private def captureIpcAddresses(handle: Ptr[Byte]): Either[EmileError.IO, (Matchable, Matchable)] =
    for
      local <- capturePipeName(LibUV.uv_pipe_getsockname, handle).left.map(IOMapping.fromCode)
      peer <- capturePipeName(LibUV.uv_pipe_getpeername, handle).left.map(IOMapping.fromCode)
    yield (local, peer)

  private def captureConnectAddresses(handle: Ptr[Byte]): Either[EmileError.Connect, (IPCAddress, IPCAddress)] =
    for
      local <- capturePipeName(LibUV.uv_pipe_getsockname, handle).left.map(ConnectMapping.fromCode)
      peer <- capturePipeName(LibUV.uv_pipe_getpeername, handle).left.map(ConnectMapping.fromCode)
    yield (local, peer)

  // FFI: handle / req calloc null-checks, uv_close cleanup paths for half-built resources.
  // scalafix:off DisableSyntax

  private def bindInstall(
    poller: LibUVPoller,
    address: IPCAddress,
    options: IPCOptions,
    queue: UnboundedQueue[IO, Either[EmileError.IO, Unit]]
  ): Either[EmileError.Bind, IPCServer] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_NAMED_PIPE))
    if handle == null then throw new OutOfMemoryError("emile: uv_pipe_t allocation failed")
    val initRc = LibUV.uv_pipe_init(poller.loop, handle, 0)
    if initRc != 0 then
      stdlib.free(handle)
      Left(BindMapping.fromCode(initRc))
    else bindAndListen(poller, handle, address, options, queue)

  private def bindAndListen(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    address: IPCAddress,
    options: IPCOptions,
    queue: UnboundedQueue[IO, Either[EmileError.IO, Unit]]
  ): Either[EmileError.Bind, IPCServer] =
    val buf = stackalloc[Byte](NameMax)
    val namelen = writeName(address, buf)
    val bindRc = LibUV.uv_pipe_bind2(handle, buf, namelen, LibUV.UV_PIPE_NO_TRUNCATE.toUInt)
    if bindRc != 0 then
      LibUV.uv_close(handle, freeHandleCb)
      Left(BindMapping.fromCode(bindRc))
    else
      capturePipeName(LibUV.uv_pipe_getsockname, handle) match
        case Left(rc) =>
          LibUV.uv_close(handle, freeHandleCb)
          Left(BindMapping.fromCode(rc))
        case Right(local) =>
          // construct stores the server in the handle's `data` slot before uv_listen activates, so a
          // connection_cb never fires on an unstored handle.
          val server = StreamServer.construct[SocketKind.IPC](handle, poller, local, queue, ipcAcceptor)
          hardenThenListen(poller, handle, options, server)
  end bindAndListen

  // chmod runs before uv_listen so a hardened socket is never briefly reachable at a wider mode;
  // validateBind has already guaranteed a mode is set only for a filesystem-path address, where
  // uv_pipe_chmod applies.
  private def hardenThenListen(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    options: IPCOptions,
    server: IPCServer
  ): Either[EmileError.Bind, IPCServer] =
    val chmodRc = options.mode match
      case Some(mode) => StreamServer.chmodHandle(handle, mode)
      case None => 0
    if chmodRc != 0 then closeAfterListenFailure(poller, handle, chmodRc)
    else
      val listenRc = LibUV.uv_listen(handle, options.listenBacklog, StreamServer.connectionCb)
      if listenRc != 0 then closeAfterListenFailure(poller, handle, listenRc)
      else Right(server)

  private def closeAfterListenFailure(poller: LibUVPoller, handle: Ptr[Byte], rc: Int): Either[EmileError.Bind, IPCServer] =
    CallbackBridge.clear(poller, handle)
    LibUV.uv_close(handle, freeHandleCb)
    Left(BindMapping.fromCode(rc))

  private def performConnect(poller: LibUVPoller, address: IPCAddress): IO[IPCSocket] =
    IO.async[IPCSocket] { cb =>
      Routing.onOwner(poller):
        val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_NAMED_PIPE))
        if handle == null then throw new OutOfMemoryError("emile: uv_pipe_t allocation failed")
        val initRc = LibUV.uv_pipe_init(poller.loop, handle, 0)
        if initRc != 0 then
          stdlib.free(handle)
          cb(Left(ConnectMapping.fromCode(initRc)))
          None
        else startConnect(poller, handle, address, cb)
    }

  private def startConnect(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    address: IPCAddress,
    cb: Either[Throwable, IPCSocket] => Unit
  ): Option[IO[Unit]] =
    val req = allocConnectReq()
    CallbackBridge.storeReq(poller, req, connectDeliver(cb, poller, handle))
    val buf = stackalloc[Byte](NameMax)
    val namelen = writeName(address, buf)
    val connectRc = LibUV.uv_pipe_connect2(req, handle, buf, namelen, LibUV.UV_PIPE_NO_TRUNCATE.toUInt, connectCb)
    if connectRc != 0 then
      CallbackBridge.releaseReq(poller, req)
      stdlib.free(req)
      LibUV.uv_close(handle, freeHandleCb)
      cb(Left(ConnectMapping.fromCode(connectRc)))
      None
    else
      poller.metrics.connectStarted()
      // Cancellation finaliser: uv_close the in-flight handle, which makes connectCb fire with
      // UV_ECANCELED (its connectDeliver, seeing uv_is_closing, frees only the req) and the close
      // callback frees the handle.
      Some(Routing.onOwner(poller)(abortConnect(handle)))
  end startConnect

  private def abortConnect(handle: Ptr[Byte]): Unit =
    if LibUV.uv_is_closing(handle) == 0 then LibUV.uv_close(handle, freeHandleCb)

  private def connectDeliver(
    cb: Either[Throwable, IPCSocket] => Unit,
    poller: LibUVPoller,
    handle: Ptr[Byte]
  ): (Int, Ptr[Byte]) => Unit =
    (status, req) =>
      CallbackBridge.releaseReq(poller, req)
      stdlib.free(req)
      if LibUV.uv_is_closing(handle) != 0 then poller.metrics.connectSettled(OpOutcome.Canceled) // cancelled: the finaliser uv_close'd the handle; cb is dead
      else if status < 0 then
        LibUV.uv_close(handle, freeHandleCb)
        poller.metrics.connectSettled(OpOutcome.Errored)
        cb(Left(ConnectMapping.fromCode(status)))
      else
        captureConnectAddresses(handle) match
          case Left(error) =>
            LibUV.uv_close(handle, freeHandleCb)
            poller.metrics.connectSettled(OpOutcome.Errored)
            cb(Left(error))
          case Right((local, peer)) =>
            poller.metrics.connectSettled(OpOutcome.Succeeded)
            cb(Right(Socket.construct[SocketKind.IPC](handle, poller, local, peer)))
  end connectDeliver

  private val connectCb: LibUV.ConnectCB = (req: Ptr[Byte], status: CInt) =>
    CallbackBridge.loadReq[(Int, Ptr[Byte]) => Unit](req).apply(status, req)

  // uv_pipe_getsockname/getpeername: a *size of 0 is an unnamed endpoint; a leading NUL is the Linux
  // abstract namespace (the name is not NUL-terminated); otherwise it is a filesystem path. Left is
  // the libuv rc.
  private def capturePipeName(
    getter: (Ptr[Byte], CString, Ptr[CSize]) => CInt,
    handle: Ptr[Byte]
  ): Either[Int, IPCAddress] =
    val buf = stackalloc[Byte](NameMax)
    val sizeCell = stackalloc[CSize]()
    !sizeCell = NameMax.toCSize
    val rc = getter(handle, buf, sizeCell)
    if rc < 0 then Left(rc)
    else
      val len = (!sizeCell).toInt
      if len == 0 then Right(IPCAddress.Path(""))
      else if buf(0) == 0.toByte then Right(IPCAddress.Abstract(readString(buf, 1, len - 1)))
      else Right(IPCAddress.Path(readString(buf, 0, len)))

  // Writes the bind2/connect2 wire form of `address` into `buf` and returns its byte length. Path ->
  // the path bytes (filesystem); Abstract -> a leading NUL then the name (Linux abstract namespace);
  // Autobind -> a leading NUL with zero length (Linux autobind). The caller has already rejected a name
  // longer than sun_path; the copy stays bounded to the buffer as a backstop.
  private def writeName(address: IPCAddress, buf: Ptr[Byte]): CSize =
    address match
      case IPCAddress.Path(value) => copyBytes(value.getBytes(StandardCharsets.UTF_8), buf, 0)
      case IPCAddress.Abstract(name) =>
        buf(0) = 0.toByte
        copyBytes(name.getBytes(StandardCharsets.UTF_8), buf, 1)
      case IPCAddress.Autobind =>
        buf(0) = 0.toByte
        0.toCSize

  private def copyBytes(bytes: Array[Byte], buf: Ptr[Byte], offset: Int): CSize =
    val n = math.min(bytes.length, NameMax - offset)
    (0 until n).foreach(i => buf(offset + i) = bytes(i))
    (offset + n).toCSize

  private def readString(buf: Ptr[Byte], offset: Int, length: Int): String =
    val bytes = new Array[Byte](length)
    (0 until length).foreach(i => bytes(i) = buf(offset + i))
    new String(bytes, StandardCharsets.UTF_8)

  private def allocConnectReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_CONNECT))
    if req == null then throw new OutOfMemoryError("emile: uv_connect_t allocation failed")
    else req

  private def acquirePair: EmIO[EmileError.IO, (IPCSocket, IPCSocket)] =
    EffIO.attempt(
      for
        poller <- LibUVPollingSystem.currentPoller
        result <- Routing.onOwner(poller)(makePair(poller))
        pair <- IO.fromEither(result)
      yield pair,
      EmileError.IO.Unexpected(_)
    )

  private def releasePair(pair: (IPCSocket, IPCSocket)): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(Socket.release(pair._1).flatMap(_ => Socket.release(pair._2)))

  // socketpair, then adopt each fd into a uv_pipe_t; the pair's endpoints are unnamed. On a partial
  // failure the already-adopted handle is uv_close'd and any un-adopted fd is closed directly.
  private def makePair(poller: LibUVPoller): Either[EmileError.IO, (IPCSocket, IPCSocket)] =
    val fds = stackalloc[CInt](2)
    val rc = LibUV.uv_socketpair(posixSocket.SOCK_STREAM, 0, fds, 0, 0)
    if rc != 0 then Left(IOMapping.fromCode(rc))
    else
      adopt(poller, fds(0)) match
        case Left(code0) =>
          unistd.close(fds(1)): Unit
          Left(IOMapping.fromCode(code0))
        case Right(handle0) =>
          adopt(poller, fds(1)) match
            case Left(code1) =>
              LibUV.uv_close(handle0, freeHandleCb)
              Left(IOMapping.fromCode(code1))
            case Right(handle1) =>
              Right(
                (
                  Socket.construct[SocketKind.IPC](handle0, poller, IPCAddress.Path(""), IPCAddress.Path("")),
                  Socket.construct[SocketKind.IPC](handle1, poller, IPCAddress.Path(""), IPCAddress.Path(""))
                )
              )
    end if
  end makePair

  // Adopts `fd` into a fresh uv_pipe_t. On failure the fd is closed (a successful adopt hands fd
  // ownership to the handle, closed by uv_close); Left carries the libuv rc.
  private def adopt(poller: LibUVPoller, fd: Int): Either[Int, Ptr[Byte]] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_NAMED_PIPE))
    if handle == null then throw new OutOfMemoryError("emile: uv_pipe_t allocation failed")
    val initRc = LibUV.uv_pipe_init(poller.loop, handle, 0)
    if initRc != 0 then
      stdlib.free(handle)
      unistd.close(fd): Unit
      Left(initRc)
    else
      val openRc = LibUV.uv_pipe_open(handle, fd)
      if openRc != 0 then
        unistd.close(fd): Unit
        LibUV.uv_close(handle, freeHandleCb)
        Left(openRc)
      else Right(handle)

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end IPC
