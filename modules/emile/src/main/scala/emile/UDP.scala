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
import scala.collection.mutable
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
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
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing
import emile.unsafe.SockAddr

/** A received datagram - its `payload` copied out of the receive buffer, so it outlives the
  * callback that produced it, paired with the `peer` it came from. The zero-copy counterpart is the
  * borrowed [[boilerplate.Slice Slice]] handed to [[UDPSocket.consume]].
  */
final case class Datagram(peer: SocketAddress[IpAddress], payload: Chunk[Byte]) derives CanEqual

// The per-socket state: the live handle, whether the handle was initialised for batched (recvmmsg)
// receive, the captured bind address, and the one shared receive buffer with its single-reader guard.
final private class UDPSocketState(
  val live: LiveHandle,
  val batched: Boolean,
  val localAddress: SocketAddress[IpAddress],
  val recvBuffer: ResizableBuffer
):
  // Single-receiver guard. Every receive mode drives the one recvBuffer and the handle's single
  // recv-callback slot, so a second concurrent receiver would corrupt the first. Owner-confined -
  // set / cleared only on the loop thread - so a plain var with no barrier.
  var receiving: Boolean = false // scalafix:ok DisableSyntax.var

/** A bound UDP socket - the portable datagram baseline over libuv's `uv_udp_t`, acquired through
  * [[UDP$ UDP]].bind. Receive, send, connect, and multicast operations are on
  * [[UDPSocket$ UDPSocket]]. For the ECN / PKTINFO / GSO / GRO / PMTUD surface a QUIC-class
  * transport needs, use [[UDPChannel]] instead.
  */
opaque type UDPSocket = UDPSocketState

/** Bind entry points for the portable [[UDPSocket]] baseline and the Linux batched [[UDPChannel]] -
  * the one companion over both UDP surfaces. Each operation runs on the worker that acquires the
  * resource; the result carries that worker's loop.
  */
object UDP:

  /** Bind a UDP socket on `address` with the default [[UDPOptions]]. */
  def bind(address: SocketAddress[IpAddress]): EmResource[EmileError.Bind, UDPSocket] =
    bind(address, UDPOptions.default)

  /** Bind a UDP socket on `address` with `options`. Binding completes during acquire, so every
    * failure - including the `ENOTSUP` a platform without `SO_REUSEPORT` returns for
    * [[UDPOptions.reusePort]] - surfaces here rather than later on the receive stream.
    */
  def bind(address: SocketAddress[IpAddress], options: UDPOptions): EmResource[EmileError.Bind, UDPSocket] =
    Resource.make[EffIO.Of[EmileError.Bind], UDPSocket](UDPSocket.bindAcquire(address, options))(socket =>
      EffIO.liftF(UDPSocket.release(socket))
    )

  /** Bind a Linux batched raw-fd [[UDPChannel]] on `address` with the default [[ChannelConfig]]. */
  def channel(address: SocketAddress[IpAddress]): EmResource[EmileError.Bind, UDPChannel] =
    channel(address, ChannelConfig.default)

  /** Bind a Linux batched raw-fd [[UDPChannel]] on `address` with `config` - the QUIC-class
    * substrate with per-packet ECN, PKTINFO, GSO / GRO, and PMTUD control. The socket, its options,
    * and the capability probe all complete during acquire.
    */
  def channel(address: SocketAddress[IpAddress], config: ChannelConfig): EmResource[EmileError.Bind, UDPChannel] =
    UDPChannel.bind(address, config)

end UDP

/** Receive, send, connection, and multicast operations for [[UDPSocket]], plus its equality,
  * factory, and reclamation. The receive modes ([[receive]], [[consume]]) share one per-socket
  * buffer, so a socket has a single receiver: starting one while another is in flight fails fast
  * with [[EmileError.IO.ConflictingOperation]]. Sending and receiving concurrently is fine - they
  * are independent directions. Every native operation reaches the handle through
  * [[emile.unsafe.LiveHandle LiveHandle]], so use after the socket's resource has released is a
  * typed [[EmileError.IO.AlreadyClosed]].
  */
object UDPSocket:

  given CanEqual[UDPSocket, UDPSocket] = CanEqual.derived

  extension (socket: UDPSocket)

    /** The address this socket is bound to - captured at bind, so an ephemeral (port 0) bind
      * reports its assigned port.
      */
    def localAddress: SocketAddress[IpAddress] = socket.localAddress

    /** A back-pressured stream of received datagrams, each copied out of the receive buffer. If the
      * consumer falls behind, libuv is paused and the kernel drops further datagrams (UDP has no
      * flow control), rather than buffering without bound; it resumes when the consumer pulls.
      */
    def receive: EmStream[EmileError.IO, Datagram] = receiveStream(socket)

    /** Receives continuously, running `f` inline on the owning loop thread with the peer address
      * and a borrowed [[boilerplate.Slice Slice]] over each datagram - the zero-copy counterpart to
      * [[receive]]. An empty datagram delivers an empty slice with its peer (distinct from the
      * drained state, which is internal). `f` must neither block - it would stall that worker's I/O -
      * nor retain its slice past returning; copy out with `slice.toArray` to persist. A `Left(e)`
      * stops the receive early.
      */
    @targetName("ext_consume")
    inline def consume[E <: Throwable](f: (SocketAddress[IpAddress], Slice) => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
      consumeAll(socket, f)

    /** Send `payload` to `to`. The buffer is held reachable across the in-flight send; a datagram
      * never partially completes. On a connected socket use the no-address [[send]].
      */
    @targetName("ext_sendTo")
    inline def send(payload: Slice, to: SocketAddress[IpAddress]): EmIO[EmileError.IO, Unit] =
      sendDatagram(socket, payload, to)

    /** Send `payload` to the connected peer - the connected-socket form of [[send]]. Fails with the
      * typed `ENOTCONN` if the socket is not connected.
      */
    @targetName("ext_send")
    inline def send(payload: Slice): EmIO[EmileError.IO, Unit] =
      sendConnected(socket, payload)

    /** Best-effort synchronous send of `payload` to `to` on the owning loop thread, with no
      * queueing. `true` when the datagram was accepted; `false` on `EAGAIN` - the send buffer is
      * full, or an asynchronous [[send]] on this socket has not yet drained on the loop turn - so
      * retry.
      */
    @targetName("ext_trySendTo")
    inline def trySend(payload: Slice, to: SocketAddress[IpAddress]): EmIO[EmileError.IO, Boolean] =
      trySendDatagram(socket, payload, to)

    /** Best-effort synchronous send of `payload` to the connected peer - the connected-socket form
      * of [[trySend]].
      */
    @targetName("ext_trySend")
    inline def trySend(payload: Slice): EmIO[EmileError.IO, Boolean] =
      trySendConnected(socket, payload)

    /** Best-effort synchronous send of a whole batch through one `sendmmsg`, returning the number
      * of leading datagrams accepted; re-drive the remainder from that index. Fewer than the batch
      * size means the send buffer filled (or, as with [[trySend]], an asynchronous [[send]] is
      * still pending); `0` on a whole-batch `EAGAIN`. A failure of the very first datagram surfaces
      * typed.
      */
    @targetName("ext_trySendBatch")
    inline def trySend(datagrams: Seq[(Slice, SocketAddress[IpAddress])]): EmIO[EmileError.IO, Int] =
      trySendBatch(socket, datagrams)

    /** Connect the socket to `peer`, so the address-free [[send]] / [[trySend]] reach it and only
      * its datagrams are received. Fails with the typed `EISCONN` if already connected.
      */
    def connect(peer: SocketAddress[IpAddress]): EmIO[EmileError.IO, Unit] =
      connectPeer(socket, peer)

    /** Remove the socket's connection, returning it to unconnected send / receive. Fails with the
      * typed `ENOTCONN` if it is not connected.
      */
    def disconnect: EmIO[EmileError.IO, Unit] = disconnectPeer(socket)

    /** The connected peer's address. Fails with the typed `ENOTCONN` if the socket is not
      * connected.
      */
    def peerAddress: EmIO[EmileError.IO, SocketAddress[IpAddress]] = peerAddressOf(socket)

    /** Join the multicast `group` on `interface` (the local interface address; `None` lets the
      * kernel choose). Datagrams sent to the group then reach this socket.
      */
    def join(group: IpAddress, interface: Option[IpAddress]): EmIO[EmileError.IO, Unit] =
      membership(socket, group, interface, LibUV.UV_JOIN_GROUP)

    /** Leave the multicast `group` on `interface` - the reverse of [[join]]. */
    def leave(group: IpAddress, interface: Option[IpAddress]): EmIO[EmileError.IO, Unit] =
      membership(socket, group, interface, LibUV.UV_LEAVE_GROUP)

    /** Join the multicast `group` on `interface`, accepting only datagrams from `source`
      * (source-specific multicast). Fails with the typed `ENOTSUP` on a platform whose libuv
      * compiled source-specific membership out.
      */
    def joinSource(group: IpAddress, source: IpAddress, interface: Option[IpAddress]): EmIO[EmileError.IO, Unit] =
      sourceMembership(socket, group, source, interface, LibUV.UV_JOIN_GROUP)

    /** Whether this socket's own multicast sends loop back to local members. */
    def setMulticastLoop(on: Boolean): EmIO[EmileError.IO, Unit] =
      optionCall(socket)(handle => LibUV.uv_udp_set_multicast_loop(handle, if on then 1 else 0))

    /** The TTL (1 to 255) on this socket's outgoing multicast datagrams. */
    def setMulticastTtl(ttl: Int): EmIO[EmileError.IO, Unit] =
      ttlCall(socket, ttl)(handle => LibUV.uv_udp_set_multicast_ttl(handle, ttl))

    /** The local `interface` address for this socket's outgoing multicast datagrams. */
    def setMulticastInterface(interface: IpAddress): EmIO[EmileError.IO, Unit] =
      addressCall(socket, interface)(LibUV.uv_udp_set_multicast_interface)

    /** Whether this socket may send to the broadcast address. */
    def setBroadcast(on: Boolean): EmIO[EmileError.IO, Unit] =
      optionCall(socket)(handle => LibUV.uv_udp_set_broadcast(handle, if on then 1 else 0))

    /** The TTL (1 to 255) on this socket's outgoing unicast datagrams. */
    def setTtl(ttl: Int): EmIO[EmileError.IO, Unit] =
      ttlCall(socket, ttl)(handle => LibUV.uv_udp_set_ttl(handle, ttl))

  end extension

  // Buffer for a single non-batched datagram: the 64 KiB max UDP payload, so UV_UDP_PARTIAL truncation
  // cannot occur and every datagram arrives whole.
  private inline val SingleRecvSize = 65536
  // Batched (recvmmsg) buffer: N max-datagram chunks, so one recvmmsg gathers up to N datagrams. libuv
  // caps its own gather at 20 chunks regardless.
  private inline val BatchedRecvChunks = 8
  private inline val BatchedRecvSize = BatchedRecvChunks * SingleRecvSize
  // Received-datagram queue depth behind the copy-out receive stream; the pending overflow absorbs a
  // recvmmsg batch that arrives while the queue is full.
  private inline val ReceiveQueueCapacity = 64

  private[emile] def bindAcquire(address: SocketAddress[IpAddress], options: UDPOptions): EmIO[EmileError.Bind, UDPSocket] =
    EffIO.attempt(
      for
        poller <- LibUVPollingSystem.currentPoller
        result <- Routing.onOwner(poller)(bindInstall(poller, address, options))
        socket <- IO.fromEither(result)
      yield socket,
      EmileError.Bind.Unexpected(_)
    )

  /** The shared reclamation: stop any in-flight receive, reclaim the handle, then free the receive
    * buffer.
    */
  private[emile] def release(socket: UDPSocket): IO[Unit] =
    Routing
      .onOwner(poller(socket))(LiveHandle.tryUse(socket.live, ())(handle => stopReceive(socket, handle)))
      .flatMap(_ => LiveHandle.closeOnOwner(socket.live))
      .flatMap(_ => IO(socket.recvBuffer.free()))

  // FFI: handle / req / sockaddr calloc null-checks, receiver var sentinels, uv_close cleanup paths,
  // C-bridge asInstanceOf recoveries.
  // scalafix:off DisableSyntax

  private def bindInstall(
    poller: LibUVPoller,
    address: SocketAddress[IpAddress],
    options: UDPOptions
  ): Either[EmileError.Bind, UDPSocket] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_UDP))
    if handle == null then throw new OutOfMemoryError("emile: uv_udp_t allocation failed")
    val initRc =
      if options.batchedReceive then LibUV.uv_udp_init_ex(poller.loop, handle, LibUV.UV_UDP_RECVMMSG.toUInt)
      else LibUV.uv_udp_init(poller.loop, handle)
    if initRc != 0 then
      stdlib.free(handle)
      Left(BindMapping.fromCode(initRc))
    else bindHandle(poller, handle, address, options)

  private def bindHandle(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    address: SocketAddress[IpAddress],
    options: UDPOptions
  ): Either[EmileError.Bind, UDPSocket] =
    val sockaddr = stdlib.calloc(1.toCSize, SockAddr.storageSize.toCSize)
    if sockaddr == null then
      LibUV.uv_close(handle, freeHandleCb)
      throw new OutOfMemoryError("emile: sockaddr allocation failed")
    SockAddr.write(address, sockaddr)
    val bindRc = LibUV.uv_udp_bind(handle, sockaddr, bindFlags(options))
    stdlib.free(sockaddr)
    if bindRc != 0 then
      LibUV.uv_close(handle, freeHandleCb)
      Left(BindMapping.fromCode(bindRc))
    else
      localAddressOf(handle) match
        case Left(rc) =>
          LibUV.uv_close(handle, freeHandleCb)
          Left(if rc == 0 then EmileError.Bind.InvalidAddress("unsupported address family") else BindMapping.fromCode(rc))
        case Right(local) =>
          val buffer = ResizableBuffer(if options.batchedReceive then BatchedRecvSize else SingleRecvSize)
          Right(new UDPSocketState(LiveHandle(poller, handle), options.batchedReceive, local, buffer))
  end bindHandle

  private def bindFlags(options: UDPOptions): CUnsignedInt =
    var flags = 0
    if options.reusePort then flags = flags | LibUV.UV_UDP_REUSEPORT
    if options.ipv6Only then flags = flags | LibUV.UV_UDP_IPV6ONLY
    flags.toUInt

  // The alloc / deliver pair a receive mode installs in the handle's data slot; the trampolines below
  // recover it. alloc receives the live handle so it can size the buffer by uv_udp_using_recvmmsg.
  final private case class UDPReceiver(
    alloc: (Ptr[Byte], Ptr[LibUV.Buf]) => Unit,
    deliver: (Ptr[Byte], CSSize, Ptr[LibUV.Buf], Ptr[Byte]) => Unit
  )

  private val allocCb: LibUV.AllocCB = (handle: Ptr[Byte], _: CSize, bufOut: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[UDPReceiver](handle).alloc(handle, bufOut)

  private val recvCb: LibUV.UDPRecvCB = (handle: Ptr[Byte], nread: CSSize, buf: Ptr[LibUV.Buf], addr: Ptr[Byte], _: CUnsignedInt) =>
    CallbackBridge.load[UDPReceiver](handle).deliver(handle, nread, buf, addr)

  // Every receive mode sizes the one buffer the same way: a full recvmmsg cluster when the handle uses
  // batched receive, else a single max datagram.
  private def datagramAlloc(state: UDPSocketState)(handle: Ptr[Byte], bufOut: Ptr[LibUV.Buf]): Unit =
    val cap = if LibUV.uv_udp_using_recvmmsg(handle) != 0 then BatchedRecvSize else SingleRecvSize
    bufOut._1 = state.recvBuffer.ensure(cap)
    bufOut._2 = cap.toCSize

  // The single-receiver guard wrapping each receive entry, mirroring the socket read guard: the claim
  // is taken on the owner thread and released on every outcome. consume runs on the union channel.
  private def withReceiving[E <: Throwable, A](state: UDPSocketState)(body: => EmIO[EmileError.IO | E, A]): EmIO[EmileError.IO | E, A] =
    val acquired: EmIO[EmileError.IO | E, Unit] = acquireReceiving(state)
    acquired.bracket(_ => body)(_ => releaseReceiving(state))

  private def acquireReceiving(state: UDPSocketState): EmIO[EmileError.IO, Unit] =
    EffIO.lift(Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, closedIo)(_ => claimReceiving(state))))

  private def claimReceiving(state: UDPSocketState): Either[EmileError.IO, Unit] =
    if state.receiving then Left(EmileError.IO.ConflictingOperation)
    else
      state.receiving = true
      Right(())

  private def releaseReceiving(state: UDPSocketState): IO[Unit] =
    Routing.onOwner(poller(state))(state.receiving = false)

  private def consumeAll[E <: Throwable](
    state: UDPSocketState,
    f: (SocketAddress[IpAddress], Slice) => Either[E, Unit]
  ): EmIO[EmileError.IO | E, Unit] =
    withReceiving(state)(consumeArm(state, f))

  private def consumeArm[E <: Throwable](
    state: UDPSocketState,
    f: (SocketAddress[IpAddress], Slice) => Either[E, Unit]
  ): EmIO[EmileError.IO | E, Unit] =
    EffIO.asyncAttempt[EmileError.IO | E, Unit](EmileError.IO.Unexpected(_)) { cb =>
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse(state.live, closedConsume(cb)): handle =>
          armReceiver(state, handle, consumeReceiver(state, cb, f))
          val rc = LibUV.uv_udp_recv_start(handle, allocCb, recvCb)
          if rc < 0 then
            clearReceiver(state, handle)
            cb(Left(IOMapping.fromCode(rc)))
            None
          else stopReceiveFinaliser(state)
    }

  private def consumeReceiver[E <: Throwable](
    state: UDPSocketState,
    cb: Either[EmileError.IO | E, Unit] => Unit,
    f: (SocketAddress[IpAddress], Slice) => Either[E, Unit]
  ): UDPReceiver =
    UDPReceiver(
      alloc = datagramAlloc(state),
      deliver = (handle, nread, buf, addr) =>
        val nreadInt = nread.toInt
        if nreadInt < 0 then
          stopReceive(state, handle)
          cb(Left(IOMapping.fromCode(nreadInt)))
        // addr == NULL is the drained / recvmmsg-free sentinel: the reused buffer needs no free, so skip.
        else if addr != null then
          SockAddr.read(addr) match
            case None => () // non-INET family: not deliverable to an ip4s peer
            case Some(peer) =>
              val outcome: Either[EmileError.IO | E, Unit] =
                try f(peer, Slice.of(buf._1, nreadInt))
                catch case t: Throwable => Left(EmileError.IO.Unexpected(t))
              outcome match
                case Right(()) => () // keep the watcher armed for the next datagram
                case left =>
                  stopReceive(state, handle)
                  cb(left)
        end if
    )

  // The mutable per-stream state for the copy-out receive: the bounded queue the fs2 stream pulls, a
  // pending overflow for a batch that arrives while the queue is full, and the pause / terminate flags.
  final private class UDPReadsState(
    val state: UDPSocketState,
    val queue: BoundedQueue[IO, Either[EmileError.IO, Datagram]]
  ):
    // Touched only on the handle's loop thread.
    val pending: mutable.Queue[Either[EmileError.IO, Datagram]] = mutable.Queue.empty
    var paused: Boolean = false
    var terminated: Boolean = false

  private def receiveStream(state: UDPSocketState): EmStream[EmileError.IO, Datagram] =
    Stream.resource(receiveResource(state)).flatMap(reads => Stream.repeatEval(receivePull(reads)))

  private def receiveResource(state: UDPSocketState): EmResource[EmileError.IO, UDPReadsState] =
    receivingResource(state).flatMap(_ => Resource.make[EffIO.Of[EmileError.IO], UDPReadsState](receiveAcquire(state))(receiveRelease))

  private def receivingResource(state: UDPSocketState): EmResource[EmileError.IO, Unit] =
    Resource.make[EffIO.Of[EmileError.IO], Unit](acquireReceiving(state))(_ => EffIO.liftF(releaseReceiving(state)))

  private def receiveAcquire(state: UDPSocketState): EmIO[EmileError.IO, UDPReadsState] =
    EffIO.lift(
      for
        queue <- BoundedQueue[IO, Either[EmileError.IO, Datagram]](ReceiveQueueCapacity)
        reads = new UDPReadsState(state, queue)
        result <- Routing.onOwner(poller(state))(receiveInstall(reads))
      yield result
    )

  private def receiveInstall(reads: UDPReadsState): Either[EmileError.IO, UDPReadsState] =
    LiveHandle.tryUse(reads.state.live, closedIo.map(_ => reads)): handle =>
      armReceiver(reads.state, handle, receiveReceiver(reads))
      val rc = LibUV.uv_udp_recv_start(handle, allocCb, recvCb)
      if rc < 0 then
        clearReceiver(reads.state, handle)
        Left(IOMapping.fromCode(rc))
      else Right(reads)

  private def receiveRelease(reads: UDPReadsState): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(Routing.onOwner(poller(reads.state))(LiveHandle.tryUse(reads.state.live, ())(handle => stopReceive(reads.state, handle))))

  private def receiveReceiver(reads: UDPReadsState): UDPReceiver =
    UDPReceiver(
      alloc = datagramAlloc(reads.state),
      deliver = (handle, nread, buf, addr) =>
        val nreadInt = nread.toInt
        if nreadInt < 0 then
          reads.terminated = true
          offerOrPend(reads, Left(IOMapping.fromCode(nreadInt)))
          LibUV.uv_udp_recv_stop(handle): Unit
        else if addr != null then
          SockAddr.read(addr).foreach { peer =>
            offerOrPend(reads, Right(Datagram(peer, Chunk.fromBytePtr(buf._1, nreadInt))))
            if reads.paused then LibUV.uv_udp_recv_stop(handle): Unit
          }
    )

  // Offer to the bounded queue; if it is full, stage in the pending overflow and mark paused, so a
  // recvmmsg batch that overruns the queue is not lost and the caller's next pull resumes it.
  private def offerOrPend(reads: UDPReadsState, item: Either[EmileError.IO, Datagram]): Unit =
    if !reads.queue.unsafeTryOffer(item) then
      reads.pending.enqueue(item)
      reads.paused = true

  private def receivePull(reads: UDPReadsState): EmIO[EmileError.IO, Datagram] =
    EffIO.lift(
      reads.queue.take.flatMap {
        case Right(dg) => Routing.onOwner(poller(reads.state))(receiveResume(reads)).as(Right(dg))
        case left => IO.pure(left)
      }
    )

  // On a pull, drain the pending overflow into the freed queue slot, then re-arm the paused watcher.
  private def receiveResume(reads: UDPReadsState): Unit =
    LiveHandle.tryUse(reads.state.live, terminateReads(reads, EmileError.IO.AlreadyClosed)): handle =>
      while reads.pending.nonEmpty && reads.queue.unsafeTryOffer(reads.pending.head) do reads.pending.dequeue(): Unit
      if reads.paused && reads.pending.isEmpty && !reads.terminated then
        reads.paused = false
        val rc = LibUV.uv_udp_recv_start(handle, allocCb, recvCb)
        if rc < 0 then terminateReads(reads, IOMapping.fromCode(rc))

  private def terminateReads(reads: UDPReadsState, err: EmileError.IO): Unit =
    if !reads.terminated then
      reads.terminated = true
      reads.queue.unsafeTryOffer(Left(err)): Unit

  private def sendDatagram(state: UDPSocketState, payload: Slice, to: SocketAddress[IpAddress]): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(state)):
          LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
            val sockaddr = stackalloc[Byte](SockAddr.storageSize.toCSize)
            SockAddr.write(to, sockaddr)
            submitSend(state, handle, payload, sockaddr, cb)
            None
      },
      EmileError.IO.Unexpected(_)
    )

  private def sendConnected(state: UDPSocketState, payload: Slice): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(state)):
          LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
            submitSend(state, handle, payload, nullPtr[Byte], cb)
            None
      },
      EmileError.IO.Unexpected(_)
    )

  // The keep-alive is the payload Slice, held reachable across the in-flight uv_udp_send exactly as
  // the stream write path holds its buffer; libuv copies the destination into the request, so the
  // stack sockaddr need not outlive the call.
  final private class UDPSendState(
    val poller: LibUVPoller,
    val cb: Either[Throwable, Unit] => Unit,
    @scala.annotation.unused val keepAlive: AnyRef
  )

  private def submitSend(
    state: UDPSocketState,
    handle: Ptr[Byte],
    payload: Slice,
    sockaddr: Ptr[Byte],
    cb: Either[Throwable, Unit] => Unit
  ): Unit =
    val req = allocSendReq()
    val bufs = stackalloc[LibUV.Buf]()
    bufs._1 = payload.unsafePtr
    bufs._2 = payload.length.toCSize
    CallbackBridge.storeReq(poller(state), req, new UDPSendState(poller(state), cb, payload))
    val rc = LibUV.uv_udp_send(req, handle, bufs, 1.toUInt, sockaddr, udpSendCb)
    if rc < 0 then
      CallbackBridge.releaseReq(poller(state), req)
      stdlib.free(req)
      cb(Left(IOMapping.fromCode(rc)))
  end submitSend

  private val udpSendCb: LibUV.UDPSendCB = (req: Ptr[Byte], status: CInt) =>
    val send = CallbackBridge.loadReq[UDPSendState](req)
    CallbackBridge.releaseReq(send.poller, req)
    stdlib.free(req)
    // A local close flushes queued sends with UV_ECANCELED; report it as AlreadyClosed, not a fault.
    if status == ErrorCode.UV_ECANCELED then send.cb(Left(EmileError.IO.AlreadyClosed))
    else if status < 0 then send.cb(Left(IOMapping.fromCode(status)))
    else send.cb(Right(()))

  private def trySendDatagram(state: UDPSocketState, payload: Slice, to: SocketAddress[IpAddress]): EmIO[EmileError.IO, Boolean] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse[Either[EmileError.IO, Boolean]](state.live, closedBool): handle =>
          val sockaddr = stackalloc[Byte](SockAddr.storageSize.toCSize)
          SockAddr.write(to, sockaddr)
          trySendResult(handle, payload, sockaddr)
    )

  private def trySendConnected(state: UDPSocketState, payload: Slice): EmIO[EmileError.IO, Boolean] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse[Either[EmileError.IO, Boolean]](state.live, closedBool)(handle => trySendResult(handle, payload, nullPtr[Byte]))
    )

  private def trySendResult(handle: Ptr[Byte], payload: Slice, sockaddr: Ptr[Byte]): Either[EmileError.IO, Boolean] =
    val bufs = stackalloc[LibUV.Buf]()
    bufs._1 = payload.unsafePtr
    bufs._2 = payload.length.toCSize
    val rc = LibUV.uv_udp_try_send(handle, bufs, 1.toUInt, sockaddr)
    // rc >= 0 is the byte count accepted; UV_EAGAIN is a full buffer or a still-pending async send.
    if rc >= 0 then Right(true)
    else if rc == ErrorCode.UV_EAGAIN then Right(false)
    else Left(IOMapping.fromCode(rc))

  private def trySendBatch(state: UDPSocketState, datagrams: Seq[(Slice, SocketAddress[IpAddress])]): EmIO[EmileError.IO, Int] =
    if datagrams.isEmpty then EffIO.succeed(0)
    else
      EffIO.lift(
        Routing.onOwner(poller(state)):
          // The handle is bound by construction, so uv_udp_try_send2's fd != -1 precondition holds -
          // the uniform pre-flight over the O3 divergence with uv_udp_try_send.
          LiveHandle.tryUse[Either[EmileError.IO, Int]](state.live, closedInt)(handle => trySend2(handle, datagrams))
      )

  private def trySend2(handle: Ptr[Byte], datagrams: Seq[(Slice, SocketAddress[IpAddress])]): Either[EmileError.IO, Int] =
    val n = datagrams.size
    // uv_udp_try_send2 reads these only during the synchronous sendmmsg, so heap blocks freed right
    // after suffice; the payload bytes stay reachable through the in-scope datagrams sequence.
    val bufBlock = stdlib.calloc(n.toCSize, sizeof[LibUV.Buf])
    val bufPtrs = stdlib.calloc(n.toCSize, sizeof[Ptr[LibUV.Buf]])
    val nbufs = stdlib.calloc(n.toCSize, sizeof[CUnsignedInt])
    val addrBlock = stdlib.calloc(n.toCSize, SockAddr.storageSize.toCSize)
    val addrPtrs = stdlib.calloc(n.toCSize, sizeof[Ptr[Byte]])
    if bufBlock == null || bufPtrs == null || nbufs == null || addrBlock == null || addrPtrs == null then
      throw new OutOfMemoryError("emile: uv_udp_try_send2 batch allocation failed")
    val bufs = bufBlock.asInstanceOf[Ptr[LibUV.Buf]]
    val bufPtrArr = bufPtrs.asInstanceOf[Ptr[Ptr[LibUV.Buf]]]
    val nbufArr = nbufs.asInstanceOf[Ptr[CUnsignedInt]]
    val addrArr = addrPtrs.asInstanceOf[Ptr[Ptr[Byte]]]
    var i = 0
    val it = datagrams.iterator
    while it.hasNext do
      val (slice, to) = it.next()
      val buf = bufs + i
      buf._1 = slice.unsafePtr
      buf._2 = slice.length.toCSize
      bufPtrArr(i) = buf
      nbufArr(i) = 1.toUInt
      val addr = addrBlock + i.toLong * SockAddr.storageSize
      SockAddr.write(to, addr)
      addrArr(i) = addr
      i += 1
    val rc = LibUV.uv_udp_try_send2(handle, n.toUInt, bufPtrArr, nbufArr, addrArr, 0.toUInt)
    stdlib.free(bufBlock)
    stdlib.free(bufPtrs)
    stdlib.free(nbufs)
    stdlib.free(addrBlock)
    stdlib.free(addrPtrs)
    // rc is the count sent; a whole-batch EAGAIN is 0 sent; a negative is a first-datagram failure.
    if rc >= 0 then Right(rc)
    else if rc == ErrorCode.UV_EAGAIN then Right(0)
    else Left(IOMapping.fromCode(rc))
  end trySend2

  private def connectPeer(state: UDPSocketState, peer: SocketAddress[IpAddress]): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse(state.live, closedIo): handle =>
          val sockaddr = stackalloc[Byte](SockAddr.storageSize.toCSize)
          SockAddr.write(peer, sockaddr)
          resultOf(LibUV.uv_udp_connect(handle, sockaddr))
    )

  private def disconnectPeer(state: UDPSocketState): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, closedIo)(handle => resultOf(LibUV.uv_udp_connect(handle, nullPtr[Byte]))))
    )

  private def peerAddressOf(state: UDPSocketState): EmIO[EmileError.IO, SocketAddress[IpAddress]] =
    EffIO.lift(Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, closedPeer)(readPeer)))

  private def readPeer(handle: Ptr[Byte]): Either[EmileError.IO, SocketAddress[IpAddress]] =
    val storage = stackalloc[Byte](SockAddr.storageSize.toCSize)
    val nameLen = stackalloc[CInt]()
    !nameLen = SockAddr.storageSize
    val rc = LibUV.uv_udp_getpeername(handle, storage, nameLen)
    if rc != 0 then Left(IOMapping.fromCode(rc))
    else SockAddr.read(storage).toRight(EmileError.IO.Unexpected(new IllegalStateException("emile: unsupported UDP address family")))

  private def membership(
    state: UDPSocketState,
    group: IpAddress,
    interface: Option[IpAddress],
    action: Int
  ): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse(state.live, closedIo): handle =>
          Zone:
            resultOf(LibUV.uv_udp_set_membership(handle, toCString(group.toString), interfaceArg(interface), action))
    )

  private def sourceMembership(
    state: UDPSocketState,
    group: IpAddress,
    source: IpAddress,
    interface: Option[IpAddress],
    action: Int
  ): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse(state.live, closedIo): handle =>
          Zone:
            resultOf(
              LibUV.uv_udp_set_source_membership(
                handle,
                toCString(group.toString),
                interfaceArg(interface),
                toCString(source.toString),
                action
              )
            )
    )

  private def addressCall(state: UDPSocketState, address: IpAddress)(call: (Ptr[Byte], CString) => CInt): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse(state.live, closedIo)(handle => Zone(resultOf(call(handle, toCString(address.toString)))))
    )

  private def optionCall(state: UDPSocketState)(call: Ptr[Byte] => CInt): EmIO[EmileError.IO, Unit] =
    EffIO.lift(Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, closedIo)(handle => resultOf(call(handle)))))

  private def ttlCall(state: UDPSocketState, ttl: Int)(call: Ptr[Byte] => CInt): EmIO[EmileError.IO, Unit] =
    if ttl < 1 || ttl > 255 then EffIO.fail(EmileError.IO.InvalidArgument(s"TTL must be between 1 and 255, was $ttl"))
    else optionCall(state)(call)

  // An interface address as a C string, or NULL for the kernel-chosen interface. Called inside a Zone.
  private def interfaceArg(interface: Option[IpAddress])(using Zone): CString =
    interface.fold(nullPtr[CChar])(a => toCString(a.toString))

  private def resultOf(rc: Int): Either[EmileError.IO, Unit] =
    if rc < 0 then Left(IOMapping.fromCode(rc)) else Right(())

  private[emile] def localAddressOf(handle: Ptr[Byte]): Either[Int, SocketAddress[IpAddress]] =
    val storage = stackalloc[Byte](SockAddr.storageSize.toCSize)
    val nameLen = stackalloc[CInt]()
    !nameLen = SockAddr.storageSize
    val rc = LibUV.uv_udp_getsockname(handle, storage, nameLen)
    if rc != 0 then Left(rc) else SockAddr.read(storage).toRight(0)

  private def armReceiver(state: UDPSocketState, handle: Ptr[Byte], receiver: UDPReceiver): Unit =
    CallbackBridge.store(poller(state), handle, receiver)

  private def clearReceiver(state: UDPSocketState, handle: Ptr[Byte]): Unit =
    CallbackBridge.clear(poller(state), handle)

  private def stopReceive(state: UDPSocketState, handle: Ptr[Byte]): Unit =
    LibUV.uv_udp_recv_stop(handle): Unit
    clearReceiver(state, handle)

  private def stopReceiveFinaliser(state: UDPSocketState): Option[IO[Unit]] =
    Some(Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, ())(handle => stopReceive(state, handle))))

  private def poller(state: UDPSocketState): LibUVPoller = LiveHandle.poller(state.live)

  private val closedIo: Either[EmileError.IO, Unit] = Left(EmileError.IO.AlreadyClosed)
  private val closedBool: Either[EmileError.IO, Boolean] = Left(EmileError.IO.AlreadyClosed)
  private val closedInt: Either[EmileError.IO, Int] = Left(EmileError.IO.AlreadyClosed)
  private val closedPeer: Either[EmileError.IO, SocketAddress[IpAddress]] = Left(EmileError.IO.AlreadyClosed)

  private def closedAsync[A](cb: Either[Throwable, A] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.IO.AlreadyClosed))
    Option.empty[IO[Unit]]

  private def closedConsume[E <: Throwable](cb: Either[EmileError.IO | E, Unit] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.IO.AlreadyClosed))
    Option.empty[IO[Unit]]

  private def allocSendReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_UDP_SEND))
    if req == null then throw new OutOfMemoryError("emile: uv_udp_send_t allocation failed")
    else req

  private def nullPtr[T]: Ptr[T] = fromRawPtr(Intrinsics.castLongToRawPtr(0L))

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end UDPSocket
