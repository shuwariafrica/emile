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
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibuvPoller
import emile.unsafe.Routing

/** Entry points for local inter-process stream sockets - Unix-domain sockets on Unix, named pipes
  * on Windows ([[SocketKind.Ipc]]). Bind a listener or connect a client over an [[IpcAddress]] (a
  * filesystem path, the Linux abstract namespace, or autobind); each operation runs on the worker
  * that acquires the resource, and the resulting [[IpcServer]] / [[IpcSocket]] carries that
  * worker's loop. The byte-stream surface is the shared one on [[Socket$ Socket]].
  */
object Ipc:

  /** The `listen(2)` backlog for an [[IpcServer]]. */
  inline val ListenBacklog = 128

  // sun_path caps near 108 bytes and libuv truncates a longer name, so 256 holds any usable name.
  private inline val NameMax = 256

  /** Bind a listening server on `address`. Binding and listening complete during acquire, so every
    * failure surfaces here rather than later on the accept stream. A filesystem-path server removes
    * its socket file on release (libuv unlinks it on close).
    */
  def bind(address: IpcAddress): EmResource[EmileError.Bind, IpcServer] =
    Resource.make[EffIO.Of[EmileError.Bind], IpcServer](bindAcquire(address))(server => EffIO.liftF(StreamServer.release(server)))

  /** Connect to the server at `address`. The connect is cancelable: a `timeout` or cancellation
    * aborts the in-flight `uv_pipe_connect2` and frees the handle. [[IpcAddress.Autobind]] is
    * bind-only and is rejected here.
    */
  def connect(address: IpcAddress): EmResource[EmileError.Connect, IpcSocket] =
    Resource.makeFull[EffIO.Of[EmileError.Connect], IpcSocket](poll => poll(connectRaw(address)))(socket =>
      EffIO.liftF(Socket.release(socket))
    )

  // Shared (not built per bind): an Ipc server carries no per-socket options, so finish is a no-op.
  private val ipcAcceptor: StreamServer.Acceptor =
    new StreamServer.Acceptor(
      handleType = LibUV.UV_NAMED_PIPE,
      initClient = (loop, client) => LibUV.uv_pipe_init(loop, client, 0),
      captureAddresses = captureIpcAddresses,
      finish = _ => EffIO.succeed(())
    )

  private def bindAcquire(address: IpcAddress): EmIO[EmileError.Bind, IpcServer] =
    address match
      // An empty path would encode to a zero-length name, which on Linux is autobind - a silent
      // surprise rather than the requested bind. Reject it; IpcAddress.Autobind is the explicit way.
      case IpcAddress.Path(p) if p.isEmpty =>
        EffIO.fail(
          EmileError.Bind.InvalidAddress("emile: an Ipc bind path must be non-empty; use IpcAddress.Autobind for an unnamed listener")
        )
      case _ =>
        EffIO.attempt(
          for
            poller <- LibuvPollingSystem.currentPoller
            queue <- UnboundedQueue[IO, Either[EmileError.Io, Unit]]
            result <- Routing.onOwner(poller)(bindInstall(poller, address, queue))
            server <- IO.fromEither(result)
          yield server,
          EmileError.Bind.Unexpected(_)
        )

  private def connectRaw(address: IpcAddress): EmIO[EmileError.Connect, IpcSocket] =
    address match
      case IpcAddress.Autobind =>
        EffIO.fail(EmileError.Connect.Unexpected(new IllegalArgumentException("emile: IpcAddress.Autobind is bind-only")))
      case named =>
        EffIO.attempt(
          for
            poller <- LibuvPollingSystem.currentPoller
            socket <- performConnect(poller, named)
          yield socket,
          EmileError.Connect.Unexpected(_)
        )

  private def captureIpcAddresses(handle: Ptr[Byte]): Either[EmileError.Io, (Matchable, Matchable)] =
    for
      local <- capturePipeName(LibUV.uv_pipe_getsockname, handle).left.map(IoMapping.fromCode)
      peer <- capturePipeName(LibUV.uv_pipe_getpeername, handle).left.map(IoMapping.fromCode)
    yield (local, peer)

  private def captureConnectAddresses(handle: Ptr[Byte]): Either[EmileError.Connect, (IpcAddress, IpcAddress)] =
    for
      local <- capturePipeName(LibUV.uv_pipe_getsockname, handle).left.map(ConnectMapping.fromCode)
      peer <- capturePipeName(LibUV.uv_pipe_getpeername, handle).left.map(ConnectMapping.fromCode)
    yield (local, peer)

  // FFI: handle / req calloc null-checks, uv_close cleanup paths for half-built resources.
  // scalafix:off DisableSyntax

  private def bindInstall(
    poller: LibuvPoller,
    address: IpcAddress,
    queue: UnboundedQueue[IO, Either[EmileError.Io, Unit]]
  ): Either[EmileError.Bind, IpcServer] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_NAMED_PIPE))
    if handle == null then throw new OutOfMemoryError("emile: uv_pipe_t allocation failed")
    val initRc = LibUV.uv_pipe_init(poller.loop, handle, 0)
    if initRc != 0 then
      stdlib.free(handle)
      Left(BindMapping.fromCode(initRc))
    else bindAndListen(poller, handle, address, queue)

  private def bindAndListen(
    poller: LibuvPoller,
    handle: Ptr[Byte],
    address: IpcAddress,
    queue: UnboundedQueue[IO, Either[EmileError.Io, Unit]]
  ): Either[EmileError.Bind, IpcServer] =
    val buf = stackalloc[Byte](NameMax)
    val namelen = writeName(address, buf)
    val bindRc = LibUV.uv_pipe_bind2(handle, buf, namelen, 0.toUInt)
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
          val server = StreamServer.construct[SocketKind.Ipc](handle, poller, local, queue, ipcAcceptor)
          val listenRc = LibUV.uv_listen(handle, ListenBacklog, StreamServer.connectionCb)
          if listenRc != 0 then
            CallbackBridge.clear(poller, handle)
            LibUV.uv_close(handle, freeHandleCb)
            Left(BindMapping.fromCode(listenRc))
          else Right(server)
    end if
  end bindAndListen

  private def performConnect(poller: LibuvPoller, address: IpcAddress): IO[IpcSocket] =
    IO.async[IpcSocket] { cb =>
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
    poller: LibuvPoller,
    handle: Ptr[Byte],
    address: IpcAddress,
    cb: Either[Throwable, IpcSocket] => Unit
  ): Option[IO[Unit]] =
    val req = allocConnectReq()
    CallbackBridge.storeReq(poller, req, connectDeliver(cb, poller, handle))
    val buf = stackalloc[Byte](NameMax)
    val namelen = writeName(address, buf)
    val connectRc = LibUV.uv_pipe_connect2(req, handle, buf, namelen, 0.toUInt, connectCb)
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
    cb: Either[Throwable, IpcSocket] => Unit,
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
        captureConnectAddresses(handle) match
          case Left(error) =>
            LibUV.uv_close(handle, freeHandleCb)
            cb(Left(error))
          case Right((local, peer)) =>
            cb(Right(Socket.construct[SocketKind.Ipc](handle, poller, local, peer)))

  private val connectCb: LibUV.ConnectCB = (req: Ptr[Byte], status: CInt) =>
    CallbackBridge.loadReq[(Int, Ptr[Byte]) => Unit](req).apply(status, req)

  // uv_pipe_getsockname/getpeername: a *size of 0 is an unnamed endpoint; a leading NUL is the Linux
  // abstract namespace (the name is not NUL-terminated); otherwise it is a filesystem path. Left is
  // the libuv rc.
  private def capturePipeName(
    getter: (Ptr[Byte], CString, Ptr[CSize]) => CInt,
    handle: Ptr[Byte]
  ): Either[Int, IpcAddress] =
    val buf = stackalloc[Byte](NameMax)
    val sizeCell = stackalloc[CSize]()
    !sizeCell = NameMax.toCSize
    val rc = getter(handle, buf, sizeCell)
    if rc < 0 then Left(rc)
    else
      val len = (!sizeCell).toInt
      if len == 0 then Right(IpcAddress.Path(""))
      else if buf(0) == 0.toByte then Right(IpcAddress.Abstract(readString(buf, 1, len - 1)))
      else Right(IpcAddress.Path(readString(buf, 0, len)))

  // Writes the bind2/connect2 wire form of `address` into `buf` and returns its byte length. Path ->
  // the path bytes (filesystem); Abstract -> a leading NUL then the name (Linux abstract namespace);
  // Autobind -> a leading NUL with zero length (Linux autobind). libuv truncates an over-long name,
  // mirrored by bounding the copy to the buffer.
  private def writeName(address: IpcAddress, buf: Ptr[Byte]): CSize =
    address match
      case IpcAddress.Path(value) => copyBytes(value.getBytes(StandardCharsets.UTF_8), buf, 0)
      case IpcAddress.Abstract(name) =>
        buf(0) = 0.toByte
        copyBytes(name.getBytes(StandardCharsets.UTF_8), buf, 1)
      case IpcAddress.Autobind =>
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

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end Ipc
