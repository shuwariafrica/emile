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
import scala.concurrent.duration.DurationInt
import scala.scalanative.libc.errno as libcErrno
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.errno as posixErrno
import scala.scalanative.posix.netinet.in
import scala.scalanative.posix.netinet.tcp
import scala.scalanative.posix.sys.socket as posixSocket
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.SocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing
import emile.unsafe.SockAddr

// The per-socket state: the shared stream state (live handle, read buffer, read/write guards) plus the
// captured local and peer addresses, reinterpreted at the socket's AddressOf[K] by the accessors.
final private class StreamSocketState(
  live: LiveHandle,
  val address: Matchable,
  val peerAddress: Matchable,
  readBuffer: ResizableBuffer
) extends StreamState(live, readBuffer)

/** The kind of a connected stream socket - the phantom tag that distinguishes a TCP socket from an
  * [[IPC$ IPC]] (Unix-domain / named-pipe) one at the type level while they share one byte-stream
  * implementation. Erased at runtime. The variants live in [[SocketKind$ SocketKind]].
  */
sealed trait SocketKind

/** The [[SocketKind]] variants. */
object SocketKind:

  /** A TCP stream socket. */
  sealed trait TCP extends SocketKind

  /** A local inter-process stream socket - a Unix-domain socket on Unix, a named pipe on Windows. */
  sealed trait IPC extends SocketKind

/** A connected stream socket, parameterised by its [[SocketKind]] - the shared byte-stream surface
  * of TCP and [[IPC$ IPC]] (Unix-domain / named-pipe) sockets. Covariant in `K`, so a [[TCPSocket]]
  * is usable as an [[AnySocket]]. Read, write, and lifecycle operations are on [[Socket$ Socket]].
  */
opaque type Socket[+K <: SocketKind] = StreamSocketState

/** A connected TCP socket, acquired through [[TCP$ TCP]] or [[StreamServer]]. */
type TCPSocket = Socket[SocketKind.TCP]

/** A connected [[IPC$ IPC]] (Unix-domain / named-pipe) stream socket. */
type IPCSocket = Socket[SocketKind.IPC]

/** A stream socket of any kind - the neutral spelling over which the shared operations resolve. */
type AnySocket = Socket[SocketKind]

/** The address type of a socket or server of kind `K` - an IP socket address for
  * [[SocketKind.TCP]], an [[IPCAddress]] for [[SocketKind.IPC]] - letting `address` / `peerAddress`
  * return the precise type per kind with no partial downcast.
  */
type AddressOf[K <: SocketKind] = K match
  case SocketKind.TCP => SocketAddress[IpAddress]
  case SocketKind.IPC => IPCAddress

/** Operations, factories, and equality for [[Socket]]. The byte-stream surface resolves once over
  * `Socket[K]` and is shared with [[Tty$ Tty]]; kind-specific operations (for example TCP's
  * [[setNoDelay]]) are separate extensions resolved only on the matching kind. Every native
  * operation reaches the raw handle through [[emile.unsafe.LiveHandle LiveHandle]], so use after
  * the socket's resource has released is a typed [[EmileError.IO.AlreadyClosed]], not a
  * use-after-free.
  *
  * The read modes ([[reads]], [[read]], [[readN]], [[consume]]) share one per-socket buffer, so a
  * socket has a single reader: starting one while another is in flight fails fast with
  * [[EmileError.IO.ConflictingOperation]]. Reading and writing concurrently is fine - they are
  * independent directions.
  */
object Socket:

  given [K <: SocketKind] => CanEqual[Socket[K], Socket[K]] = CanEqual.derived

  extension [K <: SocketKind](socket: Socket[K])

    /** The local address this socket is bound to - captured at connect / accept. */
    def address: AddressOf[K] = addrOf[K](socket.address)

    /** The peer address this socket is connected to - captured at connect / accept. */
    def peerAddress: AddressOf[K] = addrOf[K](socket.peerAddress)

    /** A back-pressured byte stream over a persistent libuv read. The watcher is armed once per
      * stream; if the consumer falls behind, libuv is paused with `uv_read_stop` and resumed when
      * the consumer pulls.
      */
    def reads: EmStream[EmileError.IO, Byte] = StreamCore.reads(socket)

    /** A pipe that writes every byte the source emits to the socket, chunk-by-chunk. */
    def writes: EmPipe[EmileError.IO, Byte, Nothing] = StreamCore.writes(socket)

    /** Half-close the write side via `uv_shutdown`; pending writes drain to the kernel first. Fails
      * with [[EmileError.IO.ConflictingOperation]] if a raw-fd [[sendFile]] is in flight, whose
      * bytes the FIN would otherwise truncate.
      */
    def endOfOutput: EmIO[EmileError.IO, Unit] =
      EffIO.attempt(shutdownWrite(socket), EmileError.IO.Unexpected(_))

    /** Stop accepting further data from the peer - `shutdown(fd, SHUT_RD)` on the underlying
      * descriptor. `ENOTCONN` is treated as success: the connection already ended.
      */
    def endOfInput: EmIO[EmileError.IO, Unit] =
      EffIO.lift(
        Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closedIo)(handle => shutdownRead(handle)))
      )

    /** Read up to `maxBytes`. `Some(chunk)` for data, `None` once the peer half-closes the write
      * side.
      */
    @targetName("ext_read")
    inline def read(maxBytes: Int): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
      StreamCore.readOnce(socket, maxBytes)

    /** Read exactly `numBytes`, accumulating across libuv reads. Fails with
      * [[EmileError.IO.EndOfStream]] if the peer half-closes before `numBytes` arrives.
      */
    @targetName("ext_readN")
    inline def readN(numBytes: Int): EmIO[EmileError.IO, Chunk[Byte]] =
      StreamCore.readNBytes(socket, numBytes)

    /** Write `chunk`. The buffer is held reachable across the in-flight `uv_write`. */
    @targetName("ext_write")
    inline def write(chunk: Chunk[Byte]): EmIO[EmileError.IO, Unit] =
      StreamCore.writeChunk(socket, chunk)

    /** Write `chunks` as one ordered, atomic `uv_write` - a single syscall gathering every buffer,
      * so a batch of small frames cannot interleave with a concurrent writer. Empty chunks are
      * skipped; each backing buffer is held reachable across the in-flight write.
      */
    @targetName("ext_writeChunks")
    inline def write(chunks: Seq[Chunk[Byte]]): EmIO[EmileError.IO, Unit] =
      StreamCore.writeChunks(socket, chunks)

    /** Reads one chunk and hands `f` a borrowed [[boilerplate.Slice Slice]] over the receive
      * buffer, sparing the copy the `Chunk`-returning read makes. The slice - and the memory it
      * views - is valid only while `f` runs, as the next read reuses the buffer, so `f` must not
      * retain it; copy out with `slice.toArray` to persist a value beyond it. `None` once the peer
      * half-closes.
      */
    @targetName("ext_readSlice")
    inline def read[E <: Throwable, A](f: Slice => EmIO[E, A]): EmIO[EmileError.IO | E, Option[A]] =
      StreamCore.readPtrOnce(socket, f)

    /** Write `slice` with no copy. The region is borrowed by the write until the effect completes,
      * so do not mutate the written range while it is in flight; an array-backed slice keeps its
      * backing array reachable for the duration, and a pointer-backed slice inherits
      * [[boilerplate.Slice Slice]]'s caller-keeps-alive contract. A reusable write scratch is a
      * codec-owned `Array[Byte]` filled then framed as `Slice.of(array, 0, n)`, its completion
      * sequenced before the next fill; see the write-path section of the README.
      */
    @targetName("ext_writeSlice")
    inline def write(slice: Slice): EmIO[EmileError.IO, Unit] =
      StreamCore.writeSlice(socket, slice)

    /** Write `slices` as one ordered, atomic `uv_write` gathering every region, so a batch cannot
      * interleave with a concurrent writer. Empty slices are skipped; each region is borrowed until
      * the effect completes, as with the single-slice `write`.
      */
    @targetName("ext_writeSlices")
    inline def write(slices: Seq[Slice]): EmIO[EmileError.IO, Unit] =
      StreamCore.writeSlices(socket, slices)

    /** Synchronous best-effort write of `slice` on the owning loop thread via `uv_try_write` - no
      * queueing, no callback. Returns the bytes accepted immediately, which may be fewer than
      * `slice.length` (or zero when the send buffer is full); write the remainder with
      * `write(slice.drop(n))`. The region is borrowed for the call only.
      */
    @targetName("ext_tryWrite")
    inline def tryWrite(slice: Slice): EmIO[EmileError.IO, Int] =
      StreamCore.tryWriteSlice(socket, slice)

    /** Zero-copy kernel-to-socket via `uv_fs_sendfile` - one best-effort syscall, returning the
      * bytes actually sent, which may be fewer than `length` (0 when the socket send buffer is
      * full). It bypasses libuv's write queue, so a concurrent [[write]] or [[endOfOutput]] on the
      * same socket fails fast with [[EmileError.IO.ConflictingOperation]] rather than interleaving
      * on the wire or truncating at the half-close (sequential `write` then `sendFile` is fine).
      * For a backpressured whole-file transfer use `file.reads.through(socket.writes)`.
      */
    @targetName("ext_sendFile")
    inline def sendFile(file: OpenFile, offset: Long, length: Long): EmIO[EmileError.IO, Long] =
      sendFileFromOpen(socket, file, offset, length)

    /** Reads continuously, running `onChunk` inline on the owning loop thread with a borrowed
      * [[boilerplate.Slice Slice]] over each chunk until end of input - the persistent form of the
      * borrowed `read`. `onChunk` therefore must neither block (it would stall that worker's I/O)
      * nor retain its slice past returning; a `Left(e)` stops the read early.
      */
    @targetName("ext_consume")
    inline def consume[E <: Throwable](onChunk: Slice => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
      StreamCore.consumeAll(socket, onChunk)

    /** Runs `thunk` on the socket's owning loop thread, so thread-unsafe C state - a stateful
      * native protocol codec, say - can be confined to the one thread that also drives this
      * socket's I/O. Like [[consume]], `thunk` must not block: it holds up that worker's I/O until
      * it returns.
      */
    @targetName("ext_onLoop")
    inline def onLoop[E <: Throwable, A](thunk: => Either[E, A]): EmIO[EmileError.IO | E, A] =
      runOnLoop(socket, thunk)

  end extension

  extension (socket: TCPSocket)

    /** Enable or disable Nagle's algorithm via `TCP_NODELAY`; disabling coalescing lowers latency
      * for small writes at the cost of more packets.
      */
    @targetName("ext_setNoDelay")
    inline def setNoDelay(enabled: Boolean): EmIO[EmileError.IO, Unit] =
      noDelayOn(socket, enabled)

    /** The live `TCP_NODELAY` state, read through `getsockopt` - the read counterpart to
      * [[setNoDelay]].
      */
    def noDelay: EmIO[EmileError.IO, Boolean] =
      optionBool(socket, in.IPPROTO_TCP, tcp.TCP_NODELAY)

    /** Configure keep-alive probing; `None` disables it. */
    @targetName("ext_setKeepAlive")
    inline def setKeepAlive(keepAlive: Option[TCPKeepAlive]): EmIO[EmileError.IO, Unit] =
      keepAliveOn(socket, keepAlive)

    /** The live keep-alive configuration - `None` when `SO_KEEPALIVE` is off, otherwise the idle,
      * interval, and probe count read back from the socket. The read counterpart to
      * [[setKeepAlive]].
      */
    def keepAlive: EmIO[EmileError.IO, Option[TCPKeepAlive]] =
      keepAliveConfig(socket)

    /** Abortively close the connection with a TCP RST rather than a graceful FIN, discarding any
      * queued output - for error paths, reverse proxies, and avoiding TIME_WAIT accumulation. The
      * socket is closed afterwards, so a later operation is a typed
      * [[EmileError.IO.AlreadyClosed]]. Fails with [[EmileError.IO.ConflictingOperation]] if a
      * [[sendFile]] is in flight or the write side is already half-closed with [[endOfOutput]],
      * neither of which libuv allows a reset to race.
      */
    def closeReset: EmIO[EmileError.IO, Unit] =
      resetConnection(socket)
  end extension

  extension (socket: IPCSocket)

    /** The credentials of the connected peer process, for a server to authorise a local client. */
    def peerCredentials: EmIO[EmileError.IO, PeerCredentials] =
      peerCredentialsOf(socket)

  /** Splice two connected sockets into a bidirectional pipe - the reverse-proxy / sidecar building
    * block. Each direction is copied, and when one side reaches end of input its half-close is
    * propagated to the other, so a shutdown on one connection reaches the other; the pipe completes
    * once both directions close, and a failure on either tears down the other. The sockets may be
    * of different kinds - a TCP front spliced to an [[IPC$ IPC]] backend, for instance.
    */
  def proxy(a: AnySocket, b: AnySocket): EmIO[EmileError.IO, Unit] =
    val aToB = a.reads.through(b.writes).compile.drain.flatMap(_ => b.endOfOutput)
    val bToA = b.reads.through(a.writes).compile.drain.flatMap(_ => a.endOfOutput)
    // absolve is the projection onto IO's channel, where a typed failure already rides, so IO.both
    // cancels the still-running sibling. Unexpected is idempotent, so re-typing wraps only a defect.
    EffIO.attempt(IO.both(aToB.absolve, bToA.absolve).void, EmileError.IO.Unexpected(_))

  /** Build a [[Socket]] of kind `K` over an already-initialised libuv handle - called once the
    * transport entry (`uv_*_init` + `uv_*_connect` or `uv_accept`) and the address capture have
    * succeeded. The stored addresses must be the [[AddressOf]] for `K`; the `address` /
    * `peerAddress` accessors reinterpret them at that type.
    */
  private[emile] def construct[K <: SocketKind](
    handle: Ptr[Byte],
    poller: LibUVPoller,
    address: Matchable,
    peerAddress: Matchable
  ): Socket[K] =
    new StreamSocketState(LiveHandle(poller, handle), address, peerAddress, ResizableBuffer(StreamCore.DefaultReadSize))

  /** The canonical release for a socket - the shared [[StreamCore$ StreamCore]] reclamation: stop
    * any in-flight read, reclaim the handle, and free the per-socket buffer.
    */
  private[emile] def release(socket: AnySocket): IO[Unit] =
    StreamCore.release(socket)

  /** Apply the per-socket TCP tuning in `options` - the one finish-socket step shared by connect
    * and accept. Takes [[AnySocket]] so the kind-erased accept finish can carry it; it is only ever
    * applied to a TCP socket.
    */
  private[emile] def applyOptions(socket: AnySocket, options: TCPOptions): EmIO[EmileError.IO, Unit] =
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
  private def poller(socket: StreamSocketState): LibUVPoller = LiveHandle.poller(socket.live)

  // The closed-branch result for a handle access that returns a typed Either.
  private val closedIo: Either[EmileError.IO, Unit] = Left(EmileError.IO.AlreadyClosed)

  // The closed-branch result for peerCredentials.
  private val closedPeerCred: Either[EmileError.IO, PeerCredentials] = Left(EmileError.IO.AlreadyClosed)

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

  // FFI: handle/req allocation null-checks, stackalloc fd cell for uv_fileno, C-bridge asInstanceOf
  // recoveries.
  // scalafix:off DisableSyntax

  private def shutdownWrite(socket: StreamSocketState): IO[Unit] =
    IO.async[Unit]: cb =>
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
          // A raw-fd sendFile is in flight outside libuv's write queue; uv_shutdown would send FIN past
          // it and truncate the stream. Fail fast rather than corrupt the transfer.
          if socket.sendFileActive then cb(Left(EmileError.IO.ConflictingOperation))
          else
            val req = allocShutdownReq()
            val rc = LibUV.uv_shutdown(req, handle, shutdownCb)
            if rc < 0 then
              stdlib.free(req)
              cb(Left(IOMapping.fromCode(rc)))
            else
              socket.outputShutdown = true
              CallbackBridge.storeReq(poller(socket), req, shutdownDeliver(poller(socket), cb))
          None

  private def shutdownDeliver(poller: LibUVPoller, cb: Either[Throwable, Unit] => Unit): (Int, Ptr[Byte]) => Unit =
    (status, req) =>
      CallbackBridge.releaseReq(poller, req)
      stdlib.free(req)
      if status < 0 then cb(Left(IOMapping.fromCode(status)))
      else cb(Right(()))

  private val shutdownCb: LibUV.ShutdownCB = (req: Ptr[Byte], status: CInt) =>
    CallbackBridge.loadReq[(Int, Ptr[Byte]) => Unit](req).apply(status, req)

  // uv_shutdown half-closes only the write side, so the read half-close is a raw shutdown(fd, SHUT_RD);
  // on Linux a libuv error is -errno, so failures still map through IOMapping.
  private def shutdownRead(handle: Ptr[Byte]): Either[EmileError.IO, Unit] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(handle, fdCell)
    if rc1 < 0 then Left(IOMapping.fromCode(rc1))
    else
      val rc2 = posixSocket.shutdown(!fdCell, 0)
      if rc2 < 0 then
        val err = libcErrno.errno
        // ENOTCONN: the connection already ended - treat the half-close as a no-op.
        if err == posixErrno.ENOTCONN then Right(())
        else Left(IOMapping.fromCode(-err))
      else Right(())

  private def resetConnection(socket: StreamSocketState): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(socket)):
          LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
            // A raw-fd sendFile would race the fd's close in the threadpool, and libuv forbids mixing a
            // reset with an in-flight uv_shutdown. Queued writes are fine: the RST discards them and
            // libuv cancels their requests (each write callback fires UV_ECANCELED).
            if socket.sendFileActive || socket.outputShutdown then
              cb(Left(EmileError.IO.ConflictingOperation))
              None
            else
              // Complete a concurrent in-flight read before stopping it and repurposing the data slot,
              // or its continuation would never fire. The local side initiated the reset, so the reader
              // sees AlreadyClosed (ConnectionReset would falsely imply the peer).
              val terminate = socket.readTerminate
              if terminate ne null then
                socket.readTerminate = null
                terminate(EmileError.IO.AlreadyClosed)
              LibUV.uv_read_stop(handle): Unit
              // The completion holder frees the handle in the close callback, as closeHandle does; the
              // reset sends a RST rather than a FIN. Any read receiver in the slot is superseded (the
              // read is stopped) and replaced at the same anchor key, so nothing leaks.
              CallbackBridge.store(poller(socket), handle, new Routing.CloseCompletion(poller(socket), cb))
              val rc = LibUV.uv_tcp_close_reset(handle, Routing.closeHandleCb)
              if rc < 0 then
                // libuv declined to close (a pending shutdown, or a refused SO_LINGER); the handle stays
                // live for the socket's own release. Drop the completion holder and surface the code.
                CallbackBridge.clear(poller(socket), handle)
                cb(Left(IOMapping.fromCode(rc)))
                None
              else
                // Mark closed so the socket's release does not uv_close the already-reset handle.
                LiveHandle.markClosed(socket.live): Unit
                None
      },
      EmileError.IO.Unexpected(_)
    )

  // scala-native's posix layer does not bind SO_PEERCRED; the value (17) is Linux's (asm-generic/socket.h).
  private inline val SoPeerCred = 17

  // struct ucred { pid_t pid; uid_t uid; gid_t gid; } - three 32-bit ints on Linux.
  private type Ucred = CStruct3[CInt, CInt, CInt]

  private def peerCredentialsOf(socket: StreamSocketState): EmIO[EmileError.IO, PeerCredentials] =
    EffIO.lift(Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closedPeerCred)(readPeerCred)))

  private def readPeerCred(handle: Ptr[Byte]): Either[EmileError.IO, PeerCredentials] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(handle, fdCell)
    if rc1 < 0 then Left(IOMapping.fromCode(rc1))
    else
      val cred = stackalloc[Ucred]()
      val len = stackalloc[posixSocket.socklen_t]()
      !len = sizeof[Ucred].toUInt
      val rc2 = posixSocket.getsockopt(!fdCell, posixSocket.SOL_SOCKET, SoPeerCred, cred.asInstanceOf[CVoidPtr], len)
      // getsockopt sets errno on failure; -errno is the libuv-style code IOMapping expects.
      if rc2 < 0 then Left(IOMapping.fromCode(-libcErrno.errno))
      else Right(PeerCredentials(cred._1, cred._2, cred._3))

  private def sendFileFromOpen(
    socket: StreamSocketState,
    file: OpenFile,
    offset: Long,
    length: Long
  ): EmIO[EmileError.IO, Long] =
    EffIO.attempt(
      IO.async[Long] { cb =>
        Routing.onOwner(poller(socket)):
          LiveHandle.tryUse(socket.live, closedAsync(cb)): handle =>
            // A queued write, another sendFile in flight, or a half-closed write side would interleave
            // with or truncate this raw-fd write (it bypasses libuv's write queue and FIN ordering).
            if socket.pendingWrites > 0 || socket.sendFileActive || socket.outputShutdown then
              cb(Left(EmileError.IO.ConflictingOperation))
              None
            else startSendFile(socket, handle, file, offset, length, cb)
      },
      EmileError.IO.Unexpected(_)
    )

  private def startSendFile(
    socket: StreamSocketState,
    handle: Ptr[Byte],
    file: OpenFile,
    offset: Long,
    length: Long,
    cb: Either[Throwable, Long] => Unit
  ): Option[IO[Unit]] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(handle, fdCell)
    if rc1 < 0 then
      cb(Left(IOMapping.fromCode(rc1)))
      None
    else
      val req = allocFsReq()
      val rc2 = LibUV.uv_fs_sendfile(poller(socket).loop, req, !fdCell, OpenFile.descriptor(file), offset, length.toCSize, fsCb)
      if rc2 < 0 then
        cleanupFsReq(req)
        cb(Left(IOMapping.fromCode(rc2)))
        None
      else
        socket.sendFileActive = true
        CallbackBridge.storeReq(poller(socket), req, sendFileDeliver(poller(socket), socket, cb))
        // Cancellation cancels the queued sendfile; its callback fires UV_ECANCELED, which
        // sendFileDeliver maps to an error and frees the request (clearing sendFileActive). Guard on
        // sendFileActive: if the deliver already ran (freed the req, cleared the flag) this is a no-op,
        // so the finaliser never uv_cancels a freed request (single-flight, so the flag pins this req).
        Some(Routing.onOwner(poller(socket)):
          if socket.sendFileActive then LibUV.uv_cancel(req): Unit)
    end if
  end startSendFile

  private def sendFileDeliver(
    poller: LibUVPoller,
    socket: StreamSocketState,
    cb: Either[Throwable, Long] => Unit
  ): Ptr[Byte] => Unit =
    req =>
      val result = LibUV.uv_fs_get_result(req).toLong
      socket.sendFileActive = false
      CallbackBridge.releaseReq(poller, req)
      cleanupFsReq(req)
      // A full send buffer surfaces as UV_EAGAIN; report it as 0 bytes sent (as tryWrite does),
      // matching the best-effort "bytes actually sent" contract.
      if result >= 0L then cb(Right(result))
      else if result.toInt == ErrorCode.UV_EAGAIN then cb(Right(0L))
      else cb(Left(IOMapping.fromCode(result.toInt)))

  private val fsCb: LibUV.FSCB = (req: Ptr[Byte]) => CallbackBridge.loadReq[Ptr[Byte] => Unit](req).apply(req)

  private def noDelayOn(socket: StreamSocketState, enabled: Boolean): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse(socket.live, closedIo): handle =>
          val rc = LibUV.uv_tcp_nodelay(handle, if enabled then 1 else 0)
          if rc < 0 then Left(IOMapping.fromCode(rc)) else Right(())
    )

  // scala-native's posix layer binds only TCP_NODELAY; these are Linux's TCP keep-alive option
  // values (netinet/tcp.h) for the probe interval and count, set by setsockopt below.
  private inline val TcpKeepIdle = 4
  private inline val TcpKeepIntvl = 5
  private inline val TcpKeepCnt = 6

  private def keepAliveOn(socket: StreamSocketState, keepAlive: Option[TCPKeepAlive]): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(socket)):
        LiveHandle.tryUse(socket.live, closedIo): handle =>
          keepAlive match
            case None => resultOf(LibUV.uv_tcp_keepalive(handle, 0, 0.toUInt))
            case Some(ka) => enableKeepAlive(handle, ka)
    )

  // uv_tcp_keepalive sets SO_KEEPALIVE and TCP_KEEPIDLE; the probe interval and count are set by the
  // setsockopt below. A sub-second idle/interval or a zero count is an argument emile rejects itself
  // (with a typed InvalidArgument), before truncating to seconds - so a sub-second window fails
  // clearly rather than rounding to zero and reaching libuv as a bare EINVAL.
  private def enableKeepAlive(handle: Ptr[Byte], ka: TCPKeepAlive): Either[EmileError.IO, Unit] =
    if ka.idle < 1.second || ka.interval < 1.second || ka.count < 1 then
      Left(EmileError.IO.InvalidArgument("keep-alive idle and interval must be at least 1 second and count at least 1"))
    else
      val rc = LibUV.uv_tcp_keepalive(handle, 1, ka.idle.toSeconds.toInt.toUInt)
      if rc < 0 then Left(IOMapping.fromCode(rc)) else setKeepAliveProbes(handle, ka)

  private def setKeepAliveProbes(handle: Ptr[Byte], ka: TCPKeepAlive): Either[EmileError.IO, Unit] =
    val fdCell = stackalloc[CInt]()
    val fdRc = LibUV.uv_fileno(handle, fdCell)
    if fdRc < 0 then Left(IOMapping.fromCode(fdRc))
    else
      val intvlRc = setKeepAliveOption(!fdCell, TcpKeepIntvl, ka.interval.toSeconds.toInt)
      if intvlRc < 0 then Left(IOMapping.fromCode(intvlRc))
      else resultOf(setKeepAliveOption(!fdCell, TcpKeepCnt, ka.count))

  // setsockopt returns 0 or -1 (setting errno); report -errno so IOMapping maps it as a libuv code.
  private def setKeepAliveOption(fd: Int, option: Int, value: Int): Int =
    val cell = stackalloc[CInt]()
    !cell = value
    val rc = posixSocket.setsockopt(fd, in.IPPROTO_TCP, option, cell.asInstanceOf[CVoidPtr], sizeof[CInt].toUInt)
    if rc < 0 then -libcErrno.errno else 0

  private def resultOf(rc: Int): Either[EmileError.IO, Unit] =
    if rc < 0 then Left(IOMapping.fromCode(rc)) else Right(())

  private def optionBool(socket: StreamSocketState, level: Int, option: Int): EmIO[EmileError.IO, Boolean] =
    EffIO.lift(Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closedBoolOption)(readOptionBool(_, level, option))))

  private val closedBoolOption: Either[EmileError.IO, Boolean] = Left(EmileError.IO.AlreadyClosed)

  private def keepAliveConfig(socket: StreamSocketState): EmIO[EmileError.IO, Option[TCPKeepAlive]] =
    EffIO.lift(Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closedKeepAlive)(readKeepAliveConfig(_))))

  private val closedKeepAlive: Either[EmileError.IO, Option[TCPKeepAlive]] = Left(EmileError.IO.AlreadyClosed)

  // getsockopt on the socket's fd - the keep-alive and TCP_NODELAY options are always valid on a
  // connected stream socket, so a failure is a genuine typed error. -errno is the libuv-style code.
  private def readOptionInt(handle: Ptr[Byte], level: Int, option: Int): Either[EmileError.IO, Int] =
    val fdCell = stackalloc[CInt]()
    val rc1 = LibUV.uv_fileno(handle, fdCell)
    if rc1 < 0 then Left(IOMapping.fromCode(rc1))
    else
      val cell = stackalloc[CInt]()
      val len = stackalloc[posixSocket.socklen_t]()
      !len = sizeof[CInt].toUInt
      val rc2 = posixSocket.getsockopt(!fdCell, level, option, cell.asInstanceOf[CVoidPtr], len)
      if rc2 < 0 then Left(IOMapping.fromCode(-libcErrno.errno))
      else Right(!cell)

  private def readOptionBool(handle: Ptr[Byte], level: Int, option: Int): Either[EmileError.IO, Boolean] =
    readOptionInt(handle, level, option).map(_ != 0)

  // The live keep-alive read: the SO_KEEPALIVE flag, then (when on) the idle/interval/count probes, so
  // the result mirrors what setKeepAlive wrote.
  private def readKeepAliveConfig(handle: Ptr[Byte]): Either[EmileError.IO, Option[TCPKeepAlive]] =
    readOptionBool(handle, posixSocket.SOL_SOCKET, posixSocket.SO_KEEPALIVE).flatMap: on =>
      if !on then Right(Option.empty[TCPKeepAlive])
      else
        for
          idle <- readOptionInt(handle, in.IPPROTO_TCP, TcpKeepIdle)
          intvl <- readOptionInt(handle, in.IPPROTO_TCP, TcpKeepIntvl)
          count <- readOptionInt(handle, in.IPPROTO_TCP, TcpKeepCnt)
        yield Some(TCPKeepAlive(idle.seconds, intvl.seconds, count))

  // onLoop touches no emile handle, so routing alone is the contract and no liveness guard applies.
  // The thunk's Left is the E arm; a NonFatal throw, or a routing failure when the loop is gone, is the
  // EmileError.IO arm.
  private def runOnLoop[E <: Throwable, A](socket: StreamSocketState, thunk: => Either[E, A]): EmIO[EmileError.IO | E, A] =
    // Guard the socket's liveness like every other operation: once released, onLoop declines rather than
    // running loop-thread code against a torn-down socket context.
    val closed: Either[EmileError.IO | E, A] = Left(EmileError.IO.AlreadyClosed)
    EffIO.lift(Routing.onOwner(poller(socket))(LiveHandle.tryUse(socket.live, closed)(_ => guardThunk(thunk))).handleError(faultLeft))

  private def guardThunk[E <: Throwable, A](thunk: => Either[E, A]): Either[EmileError.IO | E, A] =
    try thunk
    catch case scala.util.control.NonFatal(t) => Left(EmileError.IO.Unexpected(t))

  // Routing.onOwner raises on the Throwable channel only when the loop is gone; keep onLoop's failure
  // surface typed by mapping that onto the EmileError.IO arm.
  private def faultLeft[E <: Throwable, A](t: Throwable): Either[EmileError.IO | E, A] = t match
    case e: EmileError.IO => Left(e)
    case other => Left(EmileError.IO.Unexpected(other))

  // The closed-branch result for a handle access registered through IO.async: fail the callback with
  // AlreadyClosed and register no finaliser. By-name in tryUse, so it fires only when closed.
  private def closedAsync[A](cb: Either[Throwable, A] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.IO.AlreadyClosed))
    Option.empty[IO[Unit]]

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
