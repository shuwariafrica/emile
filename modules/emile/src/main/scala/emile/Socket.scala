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
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.SocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibuvPoller
import emile.unsafe.LiveHandle
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing
import emile.unsafe.SockAddr

final private class StreamSocketState(
  val live: LiveHandle,
  val address: Matchable,
  val peerAddress: Matchable,
  val readBuffer: ResizableBuffer
)

/** The kind of a connected stream socket - the phantom tag that distinguishes a TCP socket from an
  * [[Ipc$ Ipc]] (Unix-domain / named-pipe) one at the type level while they share one byte-stream
  * implementation. Erased at runtime. The variants live in [[SocketKind$ SocketKind]].
  */
sealed trait SocketKind

/** The [[SocketKind]] variants. */
object SocketKind:

  /** A TCP stream socket. */
  sealed trait Tcp extends SocketKind

  /** A local inter-process stream socket - a Unix-domain socket on Unix, a named pipe on Windows. */
  sealed trait Ipc extends SocketKind

/** A connected stream socket, parameterised by its [[SocketKind]] - the shared byte-stream surface
  * of TCP and [[Ipc$ Ipc]] (Unix-domain / named-pipe) sockets. Covariant in `K`, so a [[TcpSocket]]
  * is usable as an [[AnySocket]]. Read, write, and lifecycle operations are on [[Socket$ Socket]].
  */
opaque type Socket[+K <: SocketKind] = StreamSocketState

/** A connected TCP socket, acquired through [[Tcp$ Tcp]] or [[StreamServer]]. */
type TcpSocket = Socket[SocketKind.Tcp]

/** A connected [[Ipc$ Ipc]] (Unix-domain / named-pipe) stream socket. */
type IpcSocket = Socket[SocketKind.Ipc]

/** A stream socket of any kind - the neutral spelling over which the shared operations resolve. */
type AnySocket = Socket[SocketKind]

/** The address type of a socket or server of kind `K` - an IP socket address for
  * [[SocketKind.Tcp]], an [[IpcAddress]] for [[SocketKind.Ipc]] - letting `address` / `peerAddress`
  * return the precise type per kind with no partial downcast.
  */
type AddressOf[K <: SocketKind] = K match
  case SocketKind.Tcp => SocketAddress[IpAddress]
  case SocketKind.Ipc => IpcAddress

/** Operations, factories, and equality for [[Socket]]. The byte-stream surface is defined once over
  * `Socket[K]` and shared by every kind; kind-specific operations (for example TCP's
  * [[setNoDelay]]) are separate extensions resolved only on the matching kind. Every native
  * operation reaches the raw handle through [[emile.unsafe.LiveHandle LiveHandle]], so use after
  * the socket's resource has released is a typed [[EmileError.Io.AlreadyClosed]], not a
  * use-after-free.
  */
object Socket:

  /** Buffer size for the persistent read modes - one recv cluster without excess GC pressure. */
  inline val DefaultReadSize = 65536

  /** Back-pressure queue depth behind [[reads]]; the loop-thread staging slot absorbs the overflow
    * chunk.
    */
  inline val ReadsQueueCapacity = 4

  given [K <: SocketKind] => CanEqual[Socket[K], Socket[K]] = CanEqual.derived

  inline def read[K <: SocketKind](socket: Socket[K], maxBytes: Int): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
    socket.read(maxBytes)
  inline def readN[K <: SocketKind](socket: Socket[K], numBytes: Int): EmIO[EmileError.Io, Chunk[Byte]] =
    socket.readN(numBytes)
  inline def write[K <: SocketKind](socket: Socket[K], chunk: Chunk[Byte]): EmIO[EmileError.Io, Unit] =
    socket.write(chunk)
  inline def readPtr[K <: SocketKind, A](
    socket: Socket[K],
    f: (Ptr[Byte], Int) => EmIO[EmileError.Io, A]
  ): EmIO[EmileError.Io, Option[A]] =
    socket.readPtr(f)
  inline def writePtr[K <: SocketKind](socket: Socket[K], buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Unit] =
    socket.writePtr(buf, len)
  inline def tryWritePtr[K <: SocketKind](socket: Socket[K], buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Int] =
    socket.tryWritePtr(buf, len)
  inline def sendFile[K <: SocketKind](socket: Socket[K], file: OpenFile, offset: Long, length: Long): EmIO[EmileError.Io, Long] =
    socket.sendFile(file, offset, length)
  inline def consume[K <: SocketKind](socket: Socket[K], onChunk: (Ptr[Byte], Int) => Unit): EmIO[EmileError.Io, Unit] =
    socket.consume(onChunk)
  inline def onLoop[K <: SocketKind, A](socket: Socket[K], thunk: => A): EmIO[EmileError.Io, A] =
    socket.onLoop(thunk)
  inline def setNoDelay(socket: TcpSocket, enabled: Boolean): EmIO[EmileError.Io, Unit] =
    socket.setNoDelay(enabled)
  inline def setKeepAlive(socket: TcpSocket, keepAlive: Option[TcpKeepAlive]): EmIO[EmileError.Io, Unit] =
    socket.setKeepAlive(keepAlive)

  extension [K <: SocketKind](socket: Socket[K])

    /** The local address this socket is bound to - captured at connect / accept. */
    def address: AddressOf[K] = addrOf[K](socket.address)

    /** The peer address this socket is connected to - captured at connect / accept. */
    def peerAddress: AddressOf[K] = addrOf[K](socket.peerAddress)

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
      EffIO.lift(
        Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closedIo)(handle => shutdownRead(handle)))
      )

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

    /** Synchronous best-effort write of `len` bytes from `buf` on the owning loop thread via
      * `uv_try_write` - no queueing, no callback. Returns the bytes accepted immediately, which may
      * be fewer than `len` (or zero when the send buffer is full); write the remainder with
      * [[writePtr]]. The caller owns `buf`.
      */
    @targetName("ext_tryWritePtr")
    inline def tryWritePtr(buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Int] =
      tryWrite(socket, buf, len)

    /** Zero-copy kernel-to-socket via `uv_fs_sendfile`. */
    @targetName("ext_sendFile")
    inline def sendFile(file: OpenFile, offset: Long, length: Long): EmIO[EmileError.Io, Long] =
      sendFileFromOpen(socket, file, offset, length)

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

  extension (socket: TcpSocket)

    @targetName("ext_setNoDelay")
    inline def setNoDelay(enabled: Boolean): EmIO[EmileError.Io, Unit] =
      noDelay(socket, enabled)

    @targetName("ext_setKeepAlive")
    inline def setKeepAlive(keepAlive: Option[TcpKeepAlive]): EmIO[EmileError.Io, Unit] =
      keepAliveOn(socket, keepAlive)

  extension (socket: IpcSocket)

    /** The credentials of the connected peer process, for a server to authorise a local client. */
    def peerCredentials: EmIO[EmileError.Io, PeerCredentials] =
      peerCredentialsOf(socket)

  /** Build a [[Socket]] of kind `K` over an already-initialised libuv handle - called once the
    * transport entry (`uv_*_init` + `uv_*_connect` or `uv_accept`) and the address capture have
    * succeeded. The stored addresses must be the [[AddressOf]] for `K`; the `address` /
    * `peerAddress` accessors reinterpret them at that type.
    */
  private[emile] def construct[K <: SocketKind](
    handle: Ptr[Byte],
    poller: LibuvPoller,
    address: Matchable,
    peerAddress: Matchable
  ): Socket[K] =
    new StreamSocketState(LiveHandle(poller, handle), address, peerAddress, ResizableBuffer(DefaultReadSize))

  /** The canonical release for a socket: stop any in-flight read and clear the bridge (guarded, so
    * a redundant release is a no-op), reclaim the handle through
    * [[emile.unsafe.LiveHandle LiveHandle]], then free the per-socket buffer.
    */
  private[emile] def release(socket: AnySocket): IO[Unit] =
    Routing
      .onOwner(poller(socket))(LiveHandle.tryUse(socket.live, ())(handle => stopRead(poller(socket), handle)))
      .flatMap(_ => LiveHandle.closeOnOwner(socket.live))
      .flatMap(_ => IO(socket.readBuffer.free()))

  /** Apply the per-socket TCP tuning in `options` - the one finish-socket step shared by connect
    * and accept. Takes [[AnySocket]] so the kind-erased accept finish can carry it; it is only ever
    * applied to a TCP socket.
    */
  private[emile] def applyOptions(socket: AnySocket, options: TcpOptions): EmIO[EmileError.Io, Unit] =
    val noDelayStep = if options.noDelay then socket.setNoDelay(true) else EffIO.succeed(())
    val keepAliveStep = options.keepAlive match
      case None => EffIO.succeed(())
      case Some(ka) => socket.setKeepAlive(Some(ka))
    noDelayStep.flatMap(_ => keepAliveStep)

  /** Read the local address of a bound / connected `uv_tcp_t` handle. Run on the owner thread.
    * `Left(rc)` for a libuv error; `Left(0)` for the should-never-happen unsupported-address-family
    * result of `SockAddr.read`.
    */
  private[emile] def localAddressOf(handle: Ptr[Byte]): Either[Int, SocketAddress[IpAddress]] =
    addressOf(LibUV.uv_tcp_getsockname, handle)

  /** Read the peer address of a connected `uv_tcp_t` handle. Same contract as [[localAddressOf]]. */
  private[emile] def peerAddressOf(handle: Ptr[Byte]): Either[Int, SocketAddress[IpAddress]] =
    addressOf(LibUV.uv_tcp_getpeername, handle)

  // The owning loop's poller - always valid (it is stored alongside the handle, not freed with it).
  private def poller(socket: StreamSocketState): LibuvPoller = LiveHandle.poller(socket.live)

  // The closed-branch result for a handle access that returns a typed Either.
  private val closedIo: Either[EmileError.Io, Unit] = Left(EmileError.Io.AlreadyClosed)

  // The closed-branch result for peerCredentials.
  private val closedPeerCred: Either[EmileError.Io, PeerCredentials] = Left(EmileError.Io.AlreadyClosed)

  // phantom-guided: construct[K] stores exactly the AddressOf[K] for this kind in the address slots.
  private def addrOf[K <: SocketKind](address: Matchable): AddressOf[K] =
    address.asInstanceOf[AddressOf[K]] // scalafix:ok DisableSyntax.asInstanceOf

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
  // mode supplies its own alloc/deliver behaviour without a dedicated trampoline. deliver receives the
  // live handle libuv called back on, so a read callback never reaches for the stored (freeable) one.
  final private case class ReadReceiver(
    alloc: (CSize, Ptr[LibUV.Buf]) => Unit,
    deliver: (Ptr[Byte], CSSize, Ptr[LibUV.Buf]) => Unit
  )

  private val allocCb: LibUV.AllocCB = (handle: Ptr[Byte], suggested: CSize, bufOut: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[ReadReceiver](handle).alloc(suggested, bufOut)

  private val readCb: LibUV.ReadCB = (handle: Ptr[Byte], nread: CSSize, buf: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[ReadReceiver](handle).deliver(handle, nread, buf)

  private def readOnce(socket: StreamSocketState, maxBytes: Int): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
    EffIO.attempt(
      IO.async[Option[Chunk[Byte]]] { cb =>
        Routing.onOwner(poller(socket)):
          LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
            CallbackBridge.store(poller(socket), handle, oneShotReceiver(socket, cb, maxBytes))
            val rc = LibUV.uv_read_start(handle, allocCb, readCb)
            if rc < 0 then
              CallbackBridge.clear(poller(socket), handle)
              cb(Left(IoMapping.fromCode(rc)))
              None
            else stopReadFinaliser(socket)
      },
      EmileError.Io.Unexpected(_)
    )

  private def oneShotReceiver(
    socket: StreamSocketState,
    cb: Either[Throwable, Option[Chunk[Byte]]] => Unit,
    maxBytes: Int
  ): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = socket.readBuffer.ensure(maxBytes)
        bufOut._1 = ptr
        bufOut._2 = maxBytes.toCSize
      ,
      deliver = (handle, nread, buf) =>
        // nread == 0 is the libuv EAGAIN sentinel; wait for the next read_cb without delivering.
        if nread != 0 then
          stopRead(poller(socket), handle)
          val nreadInt = nread.toInt
          if nreadInt > 0 then cb(Right(Some(Chunk.fromBytePtr(buf._1, nreadInt))))
          else if nreadInt == ErrorCode.UV_EOF then cb(Right(None))
          else cb(Left(IoMapping.fromCode(nreadInt)))
    )

  private def readNBytes(socket: StreamSocketState, numBytes: Int): EmIO[EmileError.Io, Chunk[Byte]] =
    def go(acc: Chunk[Byte]): EmIO[EmileError.Io, Chunk[Byte]] =
      if acc.size >= numBytes then EffIO.succeed(acc)
      else
        readOnce(socket, numBytes - acc.size).flatMap:
          case Some(chunk) => go(acc ++ chunk)
          case None => EffIO.succeed(acc)
    go(Chunk.empty[Byte])

  private def readPtrOnce[A](
    socket: StreamSocketState,
    f: (Ptr[Byte], Int) => EmIO[EmileError.Io, A]
  ): EmIO[EmileError.Io, Option[A]] =
    EffIO
      .attempt(
        IO.async[Option[(Ptr[Byte], Int)]] { cb =>
          Routing.onOwner(poller(socket)):
            LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
              CallbackBridge.store(poller(socket), handle, readPtrReceiver(socket, cb))
              val rc = LibUV.uv_read_start(handle, allocCb, readCb)
              if rc < 0 then
                CallbackBridge.clear(poller(socket), handle)
                cb(Left(IoMapping.fromCode(rc)))
                None
              else stopReadFinaliser(socket)
        },
        EmileError.Io.Unexpected(_)
      )
      .flatMap:
        case Some((ptr, len)) => f(ptr, len).map(Some(_))
        case None => EffIO.succeed(None)

  private def readPtrReceiver(
    socket: StreamSocketState,
    cb: Either[Throwable, Option[(Ptr[Byte], Int)]] => Unit
  ): ReadReceiver =
    ReadReceiver(
      alloc = (suggested, bufOut) =>
        val capacity = if suggested.toInt > 0 then suggested.toInt else DefaultReadSize
        val ptr = socket.readBuffer.ensure(capacity)
        bufOut._1 = ptr
        bufOut._2 = capacity.toCSize
      ,
      deliver = (handle, nread, buf) =>
        if nread != 0 then
          stopRead(poller(socket), handle)
          val nreadInt = nread.toInt
          if nreadInt > 0 then cb(Right(Some((buf._1, nreadInt))))
          else if nreadInt == ErrorCode.UV_EOF then cb(Right(None))
          else cb(Left(IoMapping.fromCode(nreadInt)))
    )

  final private class ReadsState(
    val socket: StreamSocketState,
    val queue: BoundedQueue[IO, Either[EmileError.Io, Option[Chunk[Byte]]]]
  ):
    // Touched only on the socket's loop thread.
    var pending: Either[EmileError.Io, Option[Chunk[Byte]]] | Null = null
    var paused: Boolean = false
    var terminated: Boolean = false

  private def readsResource(socket: StreamSocketState): EmResource[EmileError.Io, ReadsState] =
    Resource.make[EffIO.Of[EmileError.Io], ReadsState](readsAcquire(socket))(readsRelease)

  private def readsAcquire(socket: StreamSocketState): EmIO[EmileError.Io, ReadsState] =
    EffIO.lift(
      for
        queue <- BoundedQueue[IO, Either[EmileError.Io, Option[Chunk[Byte]]]](ReadsQueueCapacity)
        state = new ReadsState(socket, queue)
        result <- Routing.onOwner(poller(socket))(readsInstall(state))
      yield result
    )

  private def readsInstall(state: ReadsState): Either[EmileError.Io, ReadsState] =
    LiveHandle.tryUse(state.socket.live, closedIo.map(_ => state)): handle =>
      CallbackBridge.store(poller(state.socket), handle, readsReceiver(state))
      val rc = LibUV.uv_read_start(handle, allocCb, readCb)
      if rc < 0 then
        CallbackBridge.clear(poller(state.socket), handle)
        Left(IoMapping.fromCode(rc))
      else Right(state)

  private def readsRelease(state: ReadsState): EmIO[EmileError.Io, Unit] =
    EffIO.liftF(
      Routing.onOwner(poller(state.socket))(
        LiveHandle.tryUse(state.socket.live, ())(handle => stopRead(poller(state.socket), handle))
      )
    )

  private def readsReceiver(state: ReadsState): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = state.socket.readBuffer.ensure(DefaultReadSize)
        bufOut._1 = ptr
        bufOut._2 = DefaultReadSize.toCSize
      ,
      deliver = (handle, nread, buf) =>
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
            if terminal then LibUV.uv_read_stop(handle): Unit
          else
            // Stage in the one-slot pending area and pause; the next reader pull restores it.
            state.pending = item
            state.paused = true
            LibUV.uv_read_stop(handle): Unit
    )

  private def readsPull(state: ReadsState): EmIO[EmileError.Io, Option[Chunk[Byte]]] =
    EffIO.lift(
      state.queue.take.flatMap(item => Routing.onOwner(poller(state.socket))(readsResume(state)).as(item))
    )

  // Re-arms the paused read on a pull. Guarded: if the socket released between pulls, terminate the
  // stream with a typed error rather than re-arming a freed handle.
  private def readsResume(state: ReadsState): Unit =
    LiveHandle.tryUse(state.socket.live, readsTerminate(state)): handle =>
      val pending = state.pending
      if pending ne null then
        state.pending = null
        if !state.queue.unsafeTryOffer(pending) then
          // Queue still full - re-stage and stay paused. The next pull retries.
          state.pending = pending
        else if state.paused && !state.terminated then readsRearm(state, handle)
      else if state.paused && !state.terminated then readsRearm(state, handle)

  private def readsRearm(state: ReadsState, handle: Ptr[Byte]): Unit =
    state.paused = false
    val rc = LibUV.uv_read_start(handle, allocCb, readCb)
    if rc < 0 then
      state.terminated = true
      state.queue.unsafeTryOffer(Left(IoMapping.fromCode(rc))): Unit

  private def readsTerminate(state: ReadsState): Unit =
    if !state.terminated then
      state.terminated = true
      state.queue.unsafeTryOffer(Left(EmileError.Io.AlreadyClosed)): Unit

  private def consumeAll(socket: StreamSocketState, onChunk: (Ptr[Byte], Int) => Unit): EmIO[EmileError.Io, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(socket)):
          LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
            CallbackBridge.store(poller(socket), handle, consumeReceiver(socket, cb, onChunk))
            val rc = LibUV.uv_read_start(handle, allocCb, readCb)
            if rc < 0 then
              CallbackBridge.clear(poller(socket), handle)
              cb(Left(IoMapping.fromCode(rc)))
              None
            else stopReadFinaliser(socket)
      },
      EmileError.Io.Unexpected(_)
    )

  private def consumeReceiver(
    socket: StreamSocketState,
    cb: Either[Throwable, Unit] => Unit,
    onChunk: (Ptr[Byte], Int) => Unit
  ): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = socket.readBuffer.ensure(DefaultReadSize)
        bufOut._1 = ptr
        bufOut._2 = DefaultReadSize.toCSize
      ,
      deliver = (handle, nread, buf) =>
        if nread != 0 then
          val nreadInt = nread.toInt
          if nreadInt > 0 then
            try onChunk(buf._1, nreadInt)
            catch
              case t: Throwable =>
                stopRead(poller(socket), handle)
                cb(Left(EmileError.Io.Unexpected(t)))
          else if nreadInt == ErrorCode.UV_EOF then
            stopRead(poller(socket), handle)
            cb(Right(()))
          else
            stopRead(poller(socket), handle)
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

  private def writeChunk(socket: StreamSocketState, chunk: Chunk[Byte]): EmIO[EmileError.Io, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(socket)):
          if chunk.isEmpty then
            cb(Right(()))
            None
          else
            LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
              val slice = chunk.toArraySlice
              val ptr = slice.values.atUnsafe(slice.offset)
              submitWrite(socket, handle, ptr, slice.length, cb, slice.values)
              None
      },
      EmileError.Io.Unexpected(_)
    )

  private def writeRaw(socket: StreamSocketState, buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(socket)):
          if len <= 0 then
            cb(Right(()))
            None
          else
            LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
              submitWrite(socket, handle, buf, len, cb, NoKeepAlive)
              None
      },
      EmileError.Io.Unexpected(_)
    )

  private def submitWrite(
    socket: StreamSocketState,
    handle: Ptr[Byte],
    base: Ptr[Byte],
    length: Int,
    cb: Either[Throwable, Unit] => Unit,
    keepAlive: AnyRef
  ): Unit =
    val req = allocWriteReq()
    val bufs = stackalloc[LibUV.Buf]()
    bufs._1 = base
    bufs._2 = length.toCSize
    CallbackBridge.storeReq(poller(socket), req, new WriteState(poller(socket), cb, keepAlive))
    val rc = LibUV.uv_write(req, handle, bufs, 1.toUInt, writeCb)
    if rc < 0 then
      CallbackBridge.releaseReq(poller(socket), req)
      stdlib.free(req)
      cb(Left(IoMapping.fromCode(rc)))
  end submitWrite

  private val writeCb: LibUV.WriteCB = (req: Ptr[Byte], status: CInt) =>
    val state = CallbackBridge.loadReq[WriteState](req)
    CallbackBridge.releaseReq(state.poller, req)
    stdlib.free(req)
    if status < 0 then state.cb(Left(IoMapping.fromCode(status)))
    else state.cb(Right(()))

  private def tryWrite(socket: StreamSocketState, buf: Ptr[Byte], len: Int): EmIO[EmileError.Io, Int] =
    EffIO.lift(
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse[Either[EmileError.Io, Int]](socket.live, Left(EmileError.Io.AlreadyClosed)): handle =>
          if len <= 0 then Right(0)
          else
            val bufs = stackalloc[LibUV.Buf]()
            bufs._1 = buf
            bufs._2 = len.toCSize
            val rc = LibUV.uv_try_write(handle, bufs, 1.toUInt)
            // uv_try_write returns the bytes accepted now; UV_EAGAIN means the buffer is full, so zero.
            if rc >= 0 then Right(rc)
            else if rc == ErrorCode.UV_EAGAIN then Right(0)
            else Left(IoMapping.fromCode(rc))
    )

  private def sendFileFromOpen(
    socket: StreamSocketState,
    file: OpenFile,
    offset: Long,
    length: Long
  ): EmIO[EmileError.Io, Long] =
    EffIO.attempt(
      IO.async[Long] { cb =>
        Routing.onOwner(poller(socket)):
          LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
            val fdCell = stackalloc[CInt]()
            val rc1 = LibUV.uv_fileno(handle, fdCell)
            if rc1 < 0 then
              cb(Left(IoMapping.fromCode(rc1)))
              None
            else
              val req = allocFsReq()
              val rc2 = LibUV.uv_fs_sendfile(
                poller(socket).loop,
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
                None
              else
                CallbackBridge.storeReq(poller(socket), req, sendFileDeliver(poller(socket), cb))
                // Cancellation cancels the queued sendfile; its callback fires UV_ECANCELED, which
                // sendFileDeliver maps to an error and frees the request.
                Some(Routing.onOwner(poller(socket))(LibUV.uv_cancel(req): Unit))
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

  private def noDelay(socket: StreamSocketState, enabled: Boolean): EmIO[EmileError.Io, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse(socket.live, closedIo): handle =>
          val rc = LibUV.uv_tcp_nodelay(handle, if enabled then 1 else 0)
          if rc < 0 then Left(IoMapping.fromCode(rc)) else Right(())
    )

  private def keepAliveOn(socket: StreamSocketState, keepAlive: Option[TcpKeepAlive]): EmIO[EmileError.Io, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse(socket.live, closedIo): handle =>
          val rc = keepAlive match
            case None => LibUV.uv_tcp_keepalive_ex(handle, 0, 0.toUInt, 0.toUInt, 0.toUInt)
            case Some(ka) =>
              LibUV.uv_tcp_keepalive_ex(
                handle,
                1,
                ka.idle.toSeconds.toInt.toUInt,
                ka.interval.toSeconds.toInt.toUInt,
                ka.count.toUInt
              )
          if rc < 0 then Left(IoMapping.fromCode(rc)) else Right(())
    )

  private def shutdownWrite(socket: StreamSocketState): IO[Unit] =
    IO.async[Unit]: cb =>
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
          val req = allocShutdownReq()
          val rc = LibUV.uv_shutdown(req, handle, shutdownCb)
          if rc < 0 then
            stdlib.free(req)
            cb(Left(IoMapping.fromCode(rc)))
          else CallbackBridge.storeReq(poller(socket), req, shutdownDeliver(poller(socket), cb))
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
  private def shutdownRead(handle: Ptr[Byte]): Either[EmileError.Io, Unit] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(handle, fdCell)
    if rc1 < 0 then Left(IoMapping.fromCode(rc1))
    else
      val rc2 = posixSocket.shutdown(!fdCell, 0)
      if rc2 < 0 then
        val err = libcErrno.errno
        // ENOTCONN: the connection already ended - treat the half-close as a no-op.
        if err == posixErrno.ENOTCONN then Right(())
        else Left(IoMapping.fromCode(-err))
      else Right(())

  // scala-native's posix layer does not bind SO_PEERCRED; the value (17) is Linux's (asm-generic/socket.h).
  private inline val SoPeerCred = 17

  // struct ucred { pid_t pid; uid_t uid; gid_t gid; } - three 32-bit ints on Linux.
  private type Ucred = CStruct3[CInt, CInt, CInt]

  private def peerCredentialsOf(socket: StreamSocketState): EmIO[EmileError.Io, PeerCredentials] =
    EffIO.lift(Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closedPeerCred)(readPeerCred)))

  private def readPeerCred(handle: Ptr[Byte]): Either[EmileError.Io, PeerCredentials] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(handle, fdCell)
    if rc1 < 0 then Left(IoMapping.fromCode(rc1))
    else
      val cred = stackalloc[Ucred]()
      val len = stackalloc[posixSocket.socklen_t]()
      !len = sizeof[Ucred].toUInt
      val rc2 = posixSocket.getsockopt(!fdCell, posixSocket.SOL_SOCKET, SoPeerCred, cred.asInstanceOf[CVoidPtr], len)
      // getsockopt sets errno on failure; -errno is the libuv-style code IoMapping expects.
      if rc2 < 0 then Left(IoMapping.fromCode(-libcErrno.errno))
      else Right(PeerCredentials(cred._1, cred._2, cred._3))

  // onLoop runs the consumer's thunk on the owner thread; it does not touch emile's handle, so the
  // routing alone is the contract and no liveness guard applies.
  private def runOnLoop[A](socket: StreamSocketState, thunk: => A): EmIO[EmileError.Io, A] =
    EffIO.attempt(Routing.onOwner(poller(socket))(thunk), EmileError.Io.Unexpected(_))

  // The cancellation finaliser shared by every one-shot read: stop the read on the owner, guarded so a
  // cancel that races release never touches a freed handle.
  private def stopReadFinaliser(socket: StreamSocketState): Option[IO[Unit]] =
    Some(Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, ())(handle => stopRead(poller(socket), handle))))

  private def stopRead(poller: LibuvPoller, handle: Ptr[Byte]): Unit =
    LibUV.uv_read_stop(handle): Unit
    CallbackBridge.clear(poller, handle)

  // The closed-branch result for a handle access registered through IO.async: fail the callback with
  // AlreadyClosed and register no finaliser. By-name in tryUse, so it fires only when closed.
  private def closedAsync[A](cb: Either[Throwable, A] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.Io.AlreadyClosed))
    Option.empty[IO[Unit]]

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

end Socket
