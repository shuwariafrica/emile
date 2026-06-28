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

import scala.annotation.targetName
import scala.scalanative.libc.errno as libcErrno
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.errno as posixErrno
import scala.scalanative.posix.sys.socket as posixSocket
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.BoundedQueue
import fs2.Chunk
import fs2.Stream
import com.comcast.ip4s.GenSocketAddress
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.SocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibuvPoller
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing
import emile.unsafe.SockAddr

final private class TcpSocketState(
  val handle: Ptr[Byte],
  val poller: LibuvPoller,
  val address: GenSocketAddress,
  val peerAddress: GenSocketAddress,
  val readBuffer: ResizableBuffer
)

/** A connected TCP socket, acquired through [[Tcp]] or [[TcpServer]]. Read, write, and lifecycle
  * operations are on [[TcpSocket$ TcpSocket]].
  */
opaque type TcpSocket = TcpSocketState

/** Operations, factories, and equality for [[TcpSocket]]. */
object TcpSocket:

  /** Buffer size for the persistent read modes - one recv cluster without excess GC pressure. */
  inline val DefaultReadSize = 65536

  /** Back-pressure queue depth behind [[reads]]; the loop-thread staging slot absorbs the overflow
    * chunk.
    */
  inline val ReadsQueueCapacity = 4

  given CanEqual[TcpSocket, TcpSocket] = CanEqual.derived

  inline def read(socket: TcpSocket, maxBytes: Int): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
    socket.read(maxBytes)
  inline def readN(socket: TcpSocket, numBytes: Int): EmIO[EmileError.Io, Chunk[Byte]] =
    socket.readN(numBytes)
  inline def write(socket: TcpSocket, chunk: Chunk[Byte]): EmIO[EmileError.Io, Unit] =
    socket.write(chunk)
  inline def readPtr[A](socket: TcpSocket, f: (Ptr[Byte], Int) => EmIO[EmileError.Io, A]): EmIO[EmileError.Io, Option[A]] =
    socket.readPtr(f)
  inline def writePtr(socket: TcpSocket, buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Unit] =
    socket.writePtr(buf, len)
  inline def sendFile(socket: TcpSocket, file: OpenFile, offset: Long, length: Long): EmIO[EmileError.Io, Long] =
    socket.sendFile(file, offset, length)
  inline def setNoDelay(socket: TcpSocket, enabled: Boolean): EmIO[EmileError.Io, Unit] =
    socket.setNoDelay(enabled)
  inline def setKeepAlive(socket: TcpSocket, keepAlive: Option[TcpKeepAlive]): EmIO[EmileError.Io, Unit] =
    socket.setKeepAlive(keepAlive)
  inline def consume(socket: TcpSocket, onChunk: (Ptr[Byte], Int) => Unit): EmIO[EmileError.Io, Unit] =
    socket.consume(onChunk)
  inline def onLoop[A](socket: TcpSocket, thunk: => A): EmIO[EmileError.Io, A] =
    socket.onLoop(thunk)

  extension (socket: TcpSocket)

    /** The local address this socket is bound to - captured at connect / accept. */
    def address: GenSocketAddress = socket.address

    /** The peer address this socket is connected to - captured at connect / accept. */
    def peerAddress: GenSocketAddress = socket.peerAddress

    /** A back-pressured byte stream over a persistent libuv read. The watcher is armed once per
      * stream; if the consumer falls behind, libuv is paused with `uv_read_stop` and resumed when
      * the consumer pulls.
      */
    def reads: EmStream[EmileError.Io, Byte] =
      Stream.resource(readsResource(socket)).flatMap(state => Stream.repeatEval(readsPull(state))).unNoneTerminate.unchunks

    /** A pipe that writes every byte the source emits to the socket, chunk-by-chunk. */
    def writes: EmPipe[EmileError.Io, Byte, Nothing] =
      _.chunks.foreach(chunk => socket.write(chunk))

    /** Half-close the write side via `uv_shutdown`. Pending writes drain to the kernel first. */
    def endOfOutput: EmIO[EmileError.Io, Unit] =
      EffIO.attempt(shutdownWrite(socket), EmileError.Io.Unexpected(_))

    /** Stop accepting further data from the peer - `shutdown(fd, SHUT_RD)` on the underlying
      * descriptor. `ENOTCONN` is treated as success: the connection already ended.
      */
    def endOfInput: EmIO[EmileError.Io, Unit] =
      EffIO.lift(Routing.onOwner(socket.poller)(shutdownRead(socket)))

    /** Read up to `maxBytes`. `Some(chunk)` for data, `None` once the peer half-closes the write
      * side.
      */
    @targetName("ext_read")
    inline def read(maxBytes: Int): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
      readOnce(socket, maxBytes)

    /** Read exactly `numBytes`, accumulating across libuv reads. Yields a shorter chunk if the peer
      * half-closes before `numBytes` arrives.
      */
    @targetName("ext_readN")
    inline def readN(numBytes: Int): EmIO[EmileError.Io, Chunk[Byte]] =
      readNBytes(socket, numBytes)

    /** Write `chunk`. The buffer is held reachable across the in-flight `uv_write`. */
    @targetName("ext_write")
    inline def write(chunk: Chunk[Byte]): EmIO[EmileError.Io, Unit] =
      writeChunk(socket, chunk)

    /** Zero-copy one-shot read: deliver a `(Ptr[Byte], Int)` view of one chunk to `f`. The watcher
      * is stopped before `f` runs, so the buffer is stable until the next read.
      */
    @targetName("ext_readPtr")
    inline def readPtr[A](f: (Ptr[Byte], Int) => EmIO[EmileError.Io, A]): EmIO[EmileError.Io, Option[A]] =
      readPtrOnce(socket, f)

    /** Zero-copy write of `len` bytes from `buf`. The caller owns `buf` and must keep it valid
      * until the effect completes.
      */
    @targetName("ext_writePtr")
    inline def writePtr(buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Unit] =
      writeRaw(socket, buf, len)

    /** Zero-copy kernel-to-socket via `uv_fs_sendfile`. */
    @targetName("ext_sendFile")
    inline def sendFile(file: OpenFile, offset: Long, length: Long): EmIO[EmileError.Io, Long] =
      sendFileFromOpen(socket, file, offset, length)

    @targetName("ext_setNoDelay")
    inline def setNoDelay(enabled: Boolean): EmIO[EmileError.Io, Unit] =
      noDelay(socket, enabled)

    @targetName("ext_setKeepAlive")
    inline def setKeepAlive(keepAlive: Option[TcpKeepAlive]): EmIO[EmileError.Io, Unit] =
      keepAliveOn(socket, keepAlive)

    /** Persistent zero-copy read for a synchronous loop-thread consumer. The watcher stays armed;
      * each chunk runs `onChunk` synchronously on the loop thread. `onChunk` must not block and
      * must not run effects - a throw ends the read with `EmileError.Io.Unexpected`.
      */
    @targetName("ext_consume")
    inline def consume(onChunk: (Ptr[Byte], Int) => Unit): EmIO[EmileError.Io, Unit] =
      consumeAll(socket, onChunk)

    /** Run `thunk` synchronously on the socket's owning loop thread - the public face of emile's
      * worker-affinity routing, for thread-confining consumer-side C state such as nghttp2's
      * session.
      */
    @targetName("ext_onLoop")
    inline def onLoop[A](thunk: => A): EmIO[EmileError.Io, A] =
      runOnLoop(socket, thunk)

  end extension

  /** Build a [[TcpSocket]] over an already-initialised libuv handle - called from [[Tcp$ Tcp]] and
    * [[TcpServer]] once `uv_tcp_init` (+ `uv_tcp_connect` or `uv_accept`) and the address-capture
    * have succeeded.
    */
  private[emile] def construct(
    handle: Ptr[Byte],
    poller: LibuvPoller,
    address: GenSocketAddress,
    peerAddress: GenSocketAddress
  ): TcpSocket =
    new TcpSocketState(handle, poller, address, peerAddress, ResizableBuffer(DefaultReadSize))

  /** The canonical release for a socket: stop any in-flight read, clear the bridge, free the
    * per-socket buffer, then `uv_close` and await.
    */
  private[emile] def release(socket: TcpSocket): IO[Unit] =
    Routing
      .onOwner(socket.poller):
        LibUV.uv_read_stop(socket.handle): Unit
        CallbackBridge.clear(socket.poller, socket.handle)
      .flatMap(_ => Routing.closeHandle(socket.poller, socket.handle))
      .flatMap(_ => IO(socket.readBuffer.free()))

  /** Read the local address of a bound / connected `uv_tcp_t` handle. Run on the owner thread.
    * `Left(rc)` for a libuv error; `Left(0)` for the should-never-happen unsupported-address-family
    * result of `SockAddr.read`.
    */
  private[emile] def localAddressOf(handle: Ptr[Byte]): Either[Int, SocketAddress[IpAddress]] =
    addressOf(LibUV.uv_tcp_getsockname, handle)

  /** Read the peer address of a connected `uv_tcp_t` handle. Same contract as [[localAddressOf]]. */
  private[emile] def peerAddressOf(handle: Ptr[Byte]): Either[Int, SocketAddress[IpAddress]] =
    addressOf(LibUV.uv_tcp_getpeername, handle)

  private def addressOf(
    getter: (Ptr[Byte], Ptr[Byte], Ptr[CInt]) => CInt,
    handle: Ptr[Byte]
  ): Either[Int, SocketAddress[IpAddress]] =
    val storage = stackalloc[Byte](SockAddr.storageSize.toCSize)
    val nameLen = stackalloc[CInt]()
    !nameLen = SockAddr.storageSize
    val rc = getter(handle, storage, nameLen)
    if rc != 0 then Left(rc)
    else SockAddr.read(storage).toRight(0)

  // FFI: handle/req allocation null-checks, read-receiver var/null sentinels, stackalloc fd cell
  // for uv_fileno, chunk-reachability holder, C-bridge asInstanceOf recoveries.
  // scalafix:off DisableSyntax

  // Stored in the handle's data slot; the alloc/read trampolines invoke its two closures, so each read
  // mode supplies its own alloc/deliver behaviour without a dedicated trampoline.
  final private case class ReadReceiver(
    alloc: (CSize, Ptr[LibUV.Buf]) => Unit,
    deliver: (CSSize, Ptr[LibUV.Buf]) => Unit
  )

  private val allocCb: LibUV.AllocCB = (handle: Ptr[Byte], suggested: CSize, bufOut: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[ReadReceiver](handle).alloc(suggested, bufOut)

  private val readCb: LibUV.ReadCB = (handle: Ptr[Byte], nread: CSSize, buf: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[ReadReceiver](handle).deliver(nread, buf)

  private def readOnce(socket: TcpSocket, maxBytes: Int): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
    EffIO.attempt(
      IO.async[Option[Chunk[Byte]]] { cb =>
        Routing.onOwner(socket.poller):
          CallbackBridge.store(socket.poller, socket.handle, oneShotReceiver(socket, cb, maxBytes))
          val rc = LibUV.uv_read_start(socket.handle, allocCb, readCb)
          if rc < 0 then
            CallbackBridge.clear(socket.poller, socket.handle)
            cb(Left(IoMapping.fromCode(rc)))
            None
          else Some(Routing.onOwner(socket.poller)(stopRead(socket.poller, socket.handle)))
      },
      EmileError.Io.Unexpected(_)
    )

  private def oneShotReceiver(
    socket: TcpSocket,
    cb: Either[Throwable, Option[Chunk[Byte]]] => Unit,
    maxBytes: Int
  ): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = socket.readBuffer.ensure(maxBytes)
        bufOut._1 = ptr
        bufOut._2 = maxBytes.toCSize
      ,
      deliver = (nread, buf) =>
        // nread == 0 is the libuv EAGAIN sentinel; wait for the next read_cb without delivering.
        if nread != 0 then
          stopRead(socket.poller, socket.handle)
          val nreadInt = nread.toInt
          if nreadInt > 0 then cb(Right(Some(Chunk.fromBytePtr(buf._1, nreadInt))))
          else if nreadInt == ErrorCode.UV_EOF then cb(Right(None))
          else cb(Left(IoMapping.fromCode(nreadInt)))
    )

  private def readNBytes(socket: TcpSocket, numBytes: Int): EmIO[EmileError.Io, Chunk[Byte]] =
    def go(acc: Chunk[Byte]): EmIO[EmileError.Io, Chunk[Byte]] =
      if acc.size >= numBytes then EffIO.succeed(acc)
      else
        readOnce(socket, numBytes - acc.size).flatMap:
          case Some(chunk) => go(acc ++ chunk)
          case None => EffIO.succeed(acc)
    go(Chunk.empty[Byte])

  private def readPtrOnce[A](
    socket: TcpSocket,
    f: (Ptr[Byte], Int) => EmIO[EmileError.Io, A]
  ): EmIO[EmileError.Io, Option[A]] =
    EffIO
      .attempt(
        IO.async[Option[(Ptr[Byte], Int)]] { cb =>
          Routing.onOwner(socket.poller):
            CallbackBridge.store(socket.poller, socket.handle, readPtrReceiver(socket, cb))
            val rc = LibUV.uv_read_start(socket.handle, allocCb, readCb)
            if rc < 0 then
              CallbackBridge.clear(socket.poller, socket.handle)
              cb(Left(IoMapping.fromCode(rc)))
              None
            else Some(Routing.onOwner(socket.poller)(stopRead(socket.poller, socket.handle)))
        },
        EmileError.Io.Unexpected(_)
      )
      .flatMap:
        case Some((ptr, len)) => f(ptr, len).map(Some(_))
        case None => EffIO.succeed(None)

  private def readPtrReceiver(
    socket: TcpSocket,
    cb: Either[Throwable, Option[(Ptr[Byte], Int)]] => Unit
  ): ReadReceiver =
    ReadReceiver(
      alloc = (suggested, bufOut) =>
        val capacity = if suggested.toInt > 0 then suggested.toInt else DefaultReadSize
        val ptr = socket.readBuffer.ensure(capacity)
        bufOut._1 = ptr
        bufOut._2 = capacity.toCSize
      ,
      deliver = (nread, buf) =>
        if nread != 0 then
          stopRead(socket.poller, socket.handle)
          val nreadInt = nread.toInt
          if nreadInt > 0 then cb(Right(Some((buf._1, nreadInt))))
          else if nreadInt == ErrorCode.UV_EOF then cb(Right(None))
          else cb(Left(IoMapping.fromCode(nreadInt)))
    )

  final private class ReadsState(
    val socket: TcpSocket,
    val queue: BoundedQueue[IO, Either[EmileError.Io, Option[Chunk[Byte]]]]
  ):
    // Touched only on the socket's loop thread.
    var pending: Either[EmileError.Io, Option[Chunk[Byte]]] | Null = null
    var paused: Boolean = false
    var terminated: Boolean = false

  private def readsResource(socket: TcpSocket): EmResource[EmileError.Io, ReadsState] =
    Resource.make[EffIO.Of[EmileError.Io], ReadsState](readsAcquire(socket))(readsRelease)

  private def readsAcquire(socket: TcpSocket): EmIO[EmileError.Io, ReadsState] =
    EffIO.lift(
      for
        queue <- BoundedQueue[IO, Either[EmileError.Io, Option[Chunk[Byte]]]](ReadsQueueCapacity)
        state = new ReadsState(socket, queue)
        result <- Routing.onOwner(socket.poller)(readsInstall(state))
      yield result
    )

  private def readsInstall(state: ReadsState): Either[EmileError.Io, ReadsState] =
    CallbackBridge.store(state.socket.poller, state.socket.handle, readsReceiver(state))
    val rc = LibUV.uv_read_start(state.socket.handle, allocCb, readCb)
    if rc < 0 then
      CallbackBridge.clear(state.socket.poller, state.socket.handle)
      Left(IoMapping.fromCode(rc))
    else Right(state)

  private def readsRelease(state: ReadsState): EmIO[EmileError.Io, Unit] =
    EffIO.liftF(Routing.onOwner(state.socket.poller)(stopRead(state.socket.poller, state.socket.handle)))

  private def readsReceiver(state: ReadsState): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = state.socket.readBuffer.ensure(DefaultReadSize)
        bufOut._1 = ptr
        bufOut._2 = DefaultReadSize.toCSize
      ,
      deliver = (nread, buf) =>
        if nread != 0 then
          val nreadInt = nread.toInt
          val item: Either[EmileError.Io, Option[Chunk[Byte]]] =
            if nreadInt > 0 then Right(Some(Chunk.fromBytePtr(buf._1, nreadInt)))
            else if nreadInt == ErrorCode.UV_EOF then Right(None)
            else Left(IoMapping.fromCode(nreadInt))
          val terminal = item match
            case Right(Some(_)) => false
            case _ => true
          if terminal then state.terminated = true
          if state.queue.unsafeTryOffer(item) then
            if terminal then LibUV.uv_read_stop(state.socket.handle): Unit
          else
            // Stage in the one-slot pending area and pause; the next reader pull restores it.
            state.pending = item
            state.paused = true
            LibUV.uv_read_stop(state.socket.handle): Unit
    )

  private def readsPull(state: ReadsState): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
    EffIO.lift(
      state.queue.take.flatMap(item => Routing.onOwner(state.socket.poller)(readsResume(state)).as(item))
    )

  private def readsResume(state: ReadsState): Unit =
    val pending = state.pending
    if pending ne null then
      state.pending = null
      if !state.queue.unsafeTryOffer(pending) then
        // Queue still full - re-stage and stay paused. The next pull retries.
        state.pending = pending
      else if state.paused && !state.terminated then
        state.paused = false
        val rc = LibUV.uv_read_start(state.socket.handle, allocCb, readCb)
        if rc < 0 then
          state.terminated = true
          state.queue.unsafeTryOffer(Left(IoMapping.fromCode(rc))): Unit
    else if state.paused && !state.terminated then
      state.paused = false
      val rc = LibUV.uv_read_start(state.socket.handle, allocCb, readCb)
      if rc < 0 then
        state.terminated = true
        state.queue.unsafeTryOffer(Left(IoMapping.fromCode(rc))): Unit
    end if
  end readsResume

  private def consumeAll(socket: TcpSocket, onChunk: (Ptr[Byte], Int) => Unit): EmIO[EmileError.Io, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(socket.poller):
          CallbackBridge.store(socket.poller, socket.handle, consumeReceiver(socket, cb, onChunk))
          val rc = LibUV.uv_read_start(socket.handle, allocCb, readCb)
          if rc < 0 then
            CallbackBridge.clear(socket.poller, socket.handle)
            cb(Left(IoMapping.fromCode(rc)))
            None
          else Some(Routing.onOwner(socket.poller)(stopRead(socket.poller, socket.handle)))
      },
      EmileError.Io.Unexpected(_)
    )

  private def consumeReceiver(
    socket: TcpSocket,
    cb: Either[Throwable, Unit] => Unit,
    onChunk: (Ptr[Byte], Int) => Unit
  ): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = socket.readBuffer.ensure(DefaultReadSize)
        bufOut._1 = ptr
        bufOut._2 = DefaultReadSize.toCSize
      ,
      deliver = (nread, buf) =>
        if nread != 0 then
          val nreadInt = nread.toInt
          if nreadInt > 0 then
            try onChunk(buf._1, nreadInt)
            catch
              case t: Throwable =>
                stopRead(socket.poller, socket.handle)
                cb(Left(EmileError.Io.Unexpected(t)))
          else if nreadInt == ErrorCode.UV_EOF then
            stopRead(socket.poller, socket.handle)
            cb(Right(()))
          else
            stopRead(socket.poller, socket.handle)
            cb(Left(IoMapping.fromCode(nreadInt)))
    )

  // Keeps the chunk reachable across the in-flight uv_write, and carries the poller the writeCb
  // trampoline needs to release the request's anchor.
  final private class WriteState(
    val poller: LibuvPoller,
    val cb: Either[Throwable, Unit] => Unit,
    @scala.annotation.unused val keepAlive: AnyRef
  )

  // Keep-alive sentinel for writePtr: the caller owns the buffer, so nothing of ours must stay reachable.
  private val NoKeepAlive: AnyRef = new AnyRef

  private def writeChunk(socket: TcpSocket, chunk: Chunk[Byte]): EmIO[EmileError.Io, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(socket.poller):
          if chunk.isEmpty then
            cb(Right(()))
            None
          else
            val slice = chunk.toArraySlice
            val ptr = slice.values.atUnsafe(slice.offset)
            submitWrite(socket, ptr, slice.length, cb, slice.values)
            None
      },
      EmileError.Io.Unexpected(_)
    )

  private def writeRaw(socket: TcpSocket, buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(socket.poller):
          if len <= 0 then
            cb(Right(()))
            None
          else
            submitWrite(socket, buf, len, cb, NoKeepAlive)
            None
      },
      EmileError.Io.Unexpected(_)
    )

  private def submitWrite(
    socket: TcpSocket,
    base: Ptr[Byte],
    length: Int,
    cb: Either[Throwable, Unit] => Unit,
    keepAlive: AnyRef
  ): Unit =
    val req = allocWriteReq()
    val bufs = stackalloc[LibUV.Buf]()
    bufs._1 = base
    bufs._2 = length.toCSize
    CallbackBridge.storeReq(socket.poller, req, new WriteState(socket.poller, cb, keepAlive))
    val rc = LibUV.uv_write(req, socket.handle, bufs, 1.toUInt, writeCb)
    if rc < 0 then
      CallbackBridge.releaseReq(socket.poller, req)
      stdlib.free(req)
      cb(Left(IoMapping.fromCode(rc)))
  end submitWrite

  private val writeCb: LibUV.WriteCB = (req: Ptr[Byte], status: CInt) =>
    val state = CallbackBridge.loadReq[WriteState](req)
    CallbackBridge.releaseReq(state.poller, req)
    stdlib.free(req)
    if status < 0 then state.cb(Left(IoMapping.fromCode(status)))
    else state.cb(Right(()))

  private def sendFileFromOpen(
    socket: TcpSocket,
    file: OpenFile,
    offset: Long,
    length: Long
  ): EmIO[EmileError.Io, Long] =
    EffIO.attempt(
      IO.async[Long] { cb =>
        Routing.onOwner(socket.poller):
          val fdCell = stackalloc[CInt]()
          val rc1 = LibUV.uv_fileno(socket.handle, fdCell)
          if rc1 < 0 then
            cb(Left(IoMapping.fromCode(rc1)))
            None
          else
            val req = allocFsReq()
            val rc2 = LibUV.uv_fs_sendfile(
              socket.poller.loop,
              req,
              !fdCell,
              OpenFile.descriptor(file),
              offset,
              length.toCSize,
              fsCb
            )
            if rc2 < 0 then
              cleanupFsReq(req)
              cb(Left(IoMapping.fromCode(rc2)))
            else CallbackBridge.storeReq(socket.poller, req, sendFileDeliver(socket.poller, cb))
            None
          end if
      },
      EmileError.Io.Unexpected(_)
    )

  private def sendFileDeliver(poller: LibuvPoller, cb: Either[Throwable, Long] => Unit): Ptr[Byte] => Unit =
    req =>
      val result = LibUV.uv_fs_get_result(req).toLong
      CallbackBridge.releaseReq(poller, req)
      cleanupFsReq(req)
      if result < 0L then cb(Left(IoMapping.fromCode(result.toInt)))
      else cb(Right(result))

  private val fsCb: LibUV.FsCB = (req: Ptr[Byte]) => CallbackBridge.loadReq[Ptr[Byte] => Unit](req).apply(req)

  private def noDelay(socket: TcpSocket, enabled: Boolean): EmIO[EmileError.Io, Unit] =
    EffIO.lift(
      Routing.onOwner(socket.poller):
        val rc = LibUV.uv_tcp_nodelay(socket.handle, if enabled then 1 else 0)
        if rc < 0 then Left(IoMapping.fromCode(rc)) else Right(())
    )

  private def keepAliveOn(socket: TcpSocket, keepAlive: Option[TcpKeepAlive]): EmIO[EmileError.Io, Unit] =
    EffIO.lift(
      Routing.onOwner(socket.poller):
        val rc = keepAlive match
          case None => LibUV.uv_tcp_keepalive_ex(socket.handle, 0, 0.toUInt, 0.toUInt, 0.toUInt)
          case Some(ka) =>
            LibUV.uv_tcp_keepalive_ex(
              socket.handle,
              1,
              ka.idle.toSeconds.toInt.toUInt,
              ka.interval.toSeconds.toInt.toUInt,
              ka.count.toUInt
            )
        if rc < 0 then Left(IoMapping.fromCode(rc)) else Right(())
    )

  private def shutdownWrite(socket: TcpSocket): IO[Unit] =
    IO.async[Unit]: cb =>
      Routing.onOwner(socket.poller):
        val req = allocShutdownReq()
        val rc = LibUV.uv_shutdown(req, socket.handle, shutdownCb)
        if rc < 0 then
          stdlib.free(req)
          cb(Left(IoMapping.fromCode(rc)))
        else CallbackBridge.storeReq(socket.poller, req, shutdownDeliver(socket.poller, cb))
        None

  private def shutdownDeliver(poller: LibuvPoller, cb: Either[Throwable, Unit] => Unit): (Int, Ptr[Byte]) => Unit =
    (status, req) =>
      CallbackBridge.releaseReq(poller, req)
      stdlib.free(req)
      if status < 0 then cb(Left(IoMapping.fromCode(status)))
      else cb(Right(()))

  private val shutdownCb: LibUV.ShutdownCB = (req: Ptr[Byte], status: CInt) =>
    CallbackBridge.loadReq[(Int, Ptr[Byte]) => Unit](req).apply(status, req)

  // uv_shutdown half-closes only the write side, so the read half-close is a raw shutdown(fd, SHUT_RD);
  // on Linux a libuv error is -errno, so failures still map through IoMapping.
  private def shutdownRead(socket: TcpSocket): Either[EmileError.Io, Unit] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(socket.handle, fdCell)
    if rc1 < 0 then Left(IoMapping.fromCode(rc1))
    else
      val rc2 = posixSocket.shutdown(!fdCell, 0)
      if rc2 < 0 then
        val err = libcErrno.errno
        // ENOTCONN: the connection already ended - treat the half-close as a no-op.
        if err == posixErrno.ENOTCONN then Right(())
        else Left(IoMapping.fromCode(-err))
      else Right(())

  private def runOnLoop[A](socket: TcpSocket, thunk: => A): EmIO[EmileError.Io, A] =
    EffIO.attempt(Routing.onOwner(socket.poller)(thunk), EmileError.Io.Unexpected(_))

  private def stopRead(poller: LibuvPoller, handle: Ptr[Byte]): Unit =
    LibUV.uv_read_stop(handle): Unit
    CallbackBridge.clear(poller, handle)

  private def allocWriteReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_WRITE))
    if req == null then throw new OutOfMemoryError("emile: uv_write_t allocation failed")
    else req

  private def allocShutdownReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_SHUTDOWN))
    if req == null then throw new OutOfMemoryError("emile: uv_shutdown_t allocation failed")
    else req

  private def allocFsReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_FS))
    if req == null then throw new OutOfMemoryError("emile: uv_fs_t allocation failed")
    else req

  private def cleanupFsReq(req: Ptr[Byte]): Unit =
    LibUV.uv_fs_req_cleanup(req)
    stdlib.free(req)

  // scalafix:on DisableSyntax

end TcpSocket
