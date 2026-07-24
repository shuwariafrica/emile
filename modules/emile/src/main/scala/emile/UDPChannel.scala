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
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.util.boundary
import scala.util.boundary.break

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.BoundedQueue
import fs2.Stream
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Ipv6Address
import com.comcast.ip4s.SocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.Routing
import emile.unsafe.SockAddr
import emile.unsafe.UDPSys

/** An Explicit Congestion Notification codepoint (RFC 3168 s5) - the low two bits of the IPv4 TOS /
  * IPv6 Traffic-Class octet. A transport reads it per received datagram and sets it per sent
  * datagram; the ordinal is the on-wire codepoint. Variants are on [[ECN$ ECN]].
  */
enum ECN derives CanEqual:
  /** `00` - the datagram is not using ECN. */
  case NotEct

  /** `01` - an ECN-capable transport codepoint. */
  case Ect1

  /** `10` - an ECN-capable transport codepoint; the one to use where a single codepoint suffices. */
  case Ect0

  /** `11` - Congestion Experienced, set by a router to signal congestion. */
  case Ce

/** The local address a datagram arrived on and the index of the interface that received it, from
  * `IP_PKTINFO` / `IPV6_PKTINFO`. Present on a received [[Inbound]] only when the channel enabled
  * PKTINFO. Constructed by the receive path.
  */
final case class LocalInfo(address: IpAddress, interfaceIndex: Int) derives CanEqual

/** The runtime-probed capabilities of a [[UDPChannel]]'s socket - detected at construction, since
  * GSO and GRO support and the maximum segment count vary by kernel. Constructed by the channel.
  *
  * @param gso whether the kernel accepts `UDP_SEGMENT` generic segmentation offload on send
  * @param gro whether the kernel accepts `UDP_GRO` receive coalescing
  * @param maxGsoSegments the greatest number of segments one GSO send may carry
  *   (`UDP_MAX_SEGMENTS`) - measured, not assumed, as the kernel constant has moved between 64 and
  *   128
  */
final case class ChannelCaps(gso: Boolean, gro: Boolean, maxGsoSegments: Int) derives CanEqual

/** A received datagram on a [[UDPChannel]]: the `payload` (a [[boilerplate.Slice Slice]]), the
  * `peer` it came from, its `ecn` codepoint, and, when PKTINFO is on, the `local` address and
  * interface it arrived on. In [[UDPChannel.consume]] the payload is borrowed - valid only while
  * the callback runs, as the next receive reuses the buffer - so copy out with `payload.toArray` to
  * persist it; in the copy-out [[UDPChannel.receive]] stream it is an owned copy. A GRO-coalesced
  * buffer is split into per-segment `Inbound`s, so a consumer always sees individual datagrams.
  */
final case class Inbound(payload: Slice, peer: SocketAddress[IpAddress], ecn: ECN, local: Option[LocalInfo]) derives CanEqual

/** A datagram to send on a [[UDPChannel]]: the `payload`, the destination `to`, the `ecn` codepoint
  * to mark it with, an optional `source` address to send from (via PKTINFO), and an optional
  * `gsoSegment` size to segment an oversized payload into that many-byte datagrams in one send.
  * Constructed directly. Its `payload` is borrowed only for the synchronous send.
  */
final case class Outbound(
  payload: Slice,
  to: SocketAddress[IpAddress],
  ecn: ECN,
  source: Option[IpAddress],
  gsoSegment: Option[Int]
) derives CanEqual

// The per-channel state: the uv_poll_t handle (guarded for close), the owned raw fd and its address
// family, the probed capabilities, whether PKTINFO is on, the bind address, and the persistent
// recvmmsg scatter/gather buffers with the single-receiver guard.
final private class UDPChannelState(
  val live: LiveHandle,
  val fd: Int,
  val family: Int,
  val caps: ChannelCaps,
  val localAddress: SocketAddress[IpAddress],
  val mmsgVec: Ptr[Byte],
  val iovs: Ptr[Byte],
  val names: Ptr[Byte],
  val ctls: Ptr[Byte],
  val data: Ptr[Byte]
):
  var receiving: Boolean = false // scalafix:ok DisableSyntax.var

/** The Linux batched raw-fd UDP path - the QUIC-class substrate. It owns a non-blocking
  * `SOCK_DGRAM` file descriptor and integrates with the event loop through `uv_poll` readiness
  * alone (never `uv_udp_open`, whose single-watcher-per-fd rule the two would violate). It surfaces
  * what a QUIC-class transport needs and `uv_udp` cannot: per-datagram ECN both directions, PKTINFO
  * local / source addressing, GSO send segmentation, transparent GRO receive splitting, and DF /
  * PMTUD control. Acquired through [[UDP$ UDP]].channel; operations are on
  * [[UDPChannel$ UDPChannel]]. For the portable profiles use [[UDPSocket]].
  */
opaque type UDPChannel = UDPChannelState

/** Receive, send, PMTU, and capability operations for [[UDPChannel]], plus its equality, factory,
  * and reclamation. The receive modes ([[receive]], [[consume]]) share one buffer, so a channel has
  * a single receiver: starting one while another is in flight fails fast with
  * [[EmileError.IO.ConflictingOperation]]. Every operation reaches the poll handle through
  * [[emile.unsafe.LiveHandle LiveHandle]], so use after release is a typed
  * [[EmileError.IO.AlreadyClosed]].
  */
object UDPChannel:

  given CanEqual[UDPChannel, UDPChannel] = CanEqual.derived

  extension (channel: UDPChannel)

    /** The address this channel is bound to - captured at bind, so an ephemeral (port 0) bind
      * reports its assigned port.
      */
    def localAddress: SocketAddress[IpAddress] = channel.localAddress

    /** The channel's runtime-probed [[ChannelCaps]] - GSO / GRO support and the measured maximum
      * GSO segment count.
      */
    def capabilities: ChannelCaps = channel.caps

    /** Receives continuously, running `f` inline on the owning loop thread with a borrowed
      * [[Inbound]] per datagram until `f` returns a `Left` or a socket error ends it. A
      * GRO-coalesced buffer is split into per-segment `Inbound`s. `f` must neither block - it would
      * stall that worker's I/O - nor retain its [[Inbound]]'s payload past returning; copy out with
      * `payload.toArray` to persist.
      */
    @targetName("ext_consume")
    inline def consume[E <: Throwable](f: Inbound => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
      consumeChannel(channel, f)

    /** A stream of received datagrams, each copied out so it outlives the callback - the non-hot
      * counterpart to [[consume]]. If the consumer falls behind, further datagrams are dropped (UDP
      * has no flow control), never buffered without bound.
      */
    def receive: EmStream[EmileError.IO, Inbound] = receiveStream(channel)

    /** Send `batch` in one `sendmmsg`, returning the number of leading datagrams accepted; re-drive
      * the remainder from that index. The syscall's model is lossy: a datagram after the accepted
      * prefix may have failed, its error surfacing only when re-driven from its index. A
      * first-datagram failure surfaces typed - [[EmileError.IO.MessageTooLarge]] for an oversized
      * send under DF; `0` on a whole-batch `EAGAIN`.
      */
    def sendBatch(batch: Seq[Outbound]): EmIO[EmileError.IO, Int] = sendBatchOf(channel, batch)

    /** Send every datagram in `batch`, re-driving from each failing index until all are sent. A
      * datagram that makes no progress surfaces its error typed - [[EmileError.IO.MessageTooLarge]]
      * for an oversized send, or the `EAGAIN` when the send buffer stays full.
      */
    def sendAll(batch: Seq[Outbound]): EmIO[EmileError.IO, Unit] = sendAllOf(channel, batch)

    /** Connect the channel's socket to `peer` - the single-path / client profile. Two things
      * change: [[pathMtu]] becomes readable (`IP_MTU` is only valid on a connected socket), and
      * received datagrams are restricted to `peer` (the kernel drops any other source). Sends are
      * unaffected - [[sendBatch]] / [[sendAll]] still address each datagram by its `Outbound.to`,
      * which the kernel honours on a connected UDP socket whether or not it matches `peer`.
      * Re-connecting re-points the connection to a new peer.
      */
    def connect(peer: SocketAddress[IpAddress]): EmIO[EmileError.IO, Unit] = connectPeer(channel, peer)

    /** Remove the channel's connection (a `connect` to an `AF_UNSPEC` address), returning it to
      * receiving from any peer - the multi-peer server profile - and making [[pathMtu]] a typed
      * `ENOTCONN` again.
      */
    def disconnect: EmIO[EmileError.IO, Unit] = disconnectPeer(channel)

    /** The connected peer's address. Fails with the typed `ENOTCONN` when the channel is not
      * connected.
      */
    def peerAddress: EmIO[EmileError.IO, SocketAddress[IpAddress]] = peerAddressOf(channel)

    /** Switch the Don't-Fragment / PMTU policy at runtime - the hook a DPLPMTUD state machine uses
      * to move between probing ([[PmtudMode.Probe]]) and enforcing ([[PmtudMode.Do]]).
      */
    def setPmtud(mode: PmtudMode): EmIO[EmileError.IO, Unit] = setPmtudMode(channel, mode)

    /** The current known path MTU in bytes, from `IP_MTU` / `IPV6_MTU`. It is valid only on a
      * [[connect connected]] channel; on an unconnected channel - the multi-peer server profile -
      * it is a typed `ENOTCONN`, and DPLPMTUD instead relies on [[PmtudMode.Probe]] plus
      * [[EmileError.IO.MessageTooLarge]]. Host-and-path specific, never an assumed 1500; the
      * maximum UDP payload is this less the IP and UDP headers.
      */
    def pathMtu: EmIO[EmileError.IO, Int] = pathMtuOf(channel)

  end extension

  // Batched receive: VLEN messages per recvmmsg, each with a full max-datagram data slot (a GRO
  // coalesced buffer is one receive, up to the 64 KiB max) and its own control block for the ECN /
  // PKTINFO / GRO cmsgs.
  private inline val VLEN = 16
  private inline val PerMsgData = 65536
  private inline val PerMsgCtl = 256
  // Send control block per datagram: an ECN cmsg (24) + an IPv6 PKTINFO cmsg (40) + a GSO cmsg (24).
  private inline val SendCtlMax = 128
  private inline val ReceiveQueueCapacity = 256

  private[emile] def bind(address: SocketAddress[IpAddress], config: ChannelConfig): EmResource[EmileError.Bind, UDPChannel] =
    Resource.make[EffIO.Of[EmileError.Bind], UDPChannel](bindAcquire(address, config))(channel => EffIO.liftF(release(channel)))

  private def bindAcquire(address: SocketAddress[IpAddress], config: ChannelConfig): EmIO[EmileError.Bind, UDPChannel] =
    EffIO.attempt(
      for
        poller <- LibUVPollingSystem.currentPoller
        result <- Routing.onOwner(poller)(construct(poller, address, config))
        channel <- IO.fromEither(result)
      yield channel,
      EmileError.Bind.Unexpected(_)
    )

  // FFI: fd / handle / buffer allocation and null-checks, cmsg pointer arithmetic, recvmmsg / sendmmsg
  // vector construction, C-bridge asInstanceOf recoveries.
  // scalafix:off DisableSyntax

  private def construct(
    poller: LibUVPoller,
    address: SocketAddress[IpAddress],
    config: ChannelConfig
  ): Either[EmileError.Bind, UDPChannel] =
    val family = address.host.fold(_ => UDPSys.AF_INET, _ => UDPSys.AF_INET6)
    val fd = UDPSys.socket(family, UDPSys.SOCK_DGRAM | UDPSys.SOCK_NONBLOCK | UDPSys.SOCK_CLOEXEC, 0)
    if fd < 0 then Left(BindMapping.fromCode(-libcErrno.errno))
    else
      configureSocket(fd, family, config) match
        case Left(error) =>
          UDPSys.close(fd): Unit
          Left(error)
        case Right(()) => bindSocket(poller, fd, family, address, config)

  private def bindSocket(
    poller: LibUVPoller,
    fd: Int,
    family: Int,
    address: SocketAddress[IpAddress],
    config: ChannelConfig
  ): Either[EmileError.Bind, UDPChannel] =
    val sockaddr = stackalloc[Byte](SockAddr.storageSize.toCSize)
    SockAddr.write(address, sockaddr)
    val bindRc = UDPSys.bind(fd, sockaddr, sockaddrLen(address).toUInt)
    if bindRc != 0 then
      val err = -libcErrno.errno
      UDPSys.close(fd): Unit
      Left(BindMapping.fromCode(err))
    else
      readLocalAddress(fd) match
        case Left(rc) =>
          UDPSys.close(fd): Unit
          Left(if rc == 0 then EmileError.Bind.InvalidAddress("unsupported address family") else BindMapping.fromCode(rc))
        case Right(local) => installPoll(poller, fd, family, local, config)
  end bindSocket

  private def installPoll(
    poller: LibUVPoller,
    fd: Int,
    family: Int,
    local: SocketAddress[IpAddress],
    config: ChannelConfig
  ): Either[EmileError.Bind, UDPChannel] =
    val caps = probeCaps(fd, config)
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_POLL))
    if handle == null then
      UDPSys.close(fd): Unit
      throw new OutOfMemoryError("emile: uv_poll_t allocation failed")
    val rc = LibUV.uv_poll_init(poller.loop, handle, fd)
    if rc != 0 then
      stdlib.free(handle)
      UDPSys.close(fd): Unit
      Left(BindMapping.fromCode(rc))
    else
      val state = allocateState(LiveHandle(poller, handle), fd, family, caps, local)
      Right(state)
  end installPoll

  private def allocateState(
    live: LiveHandle,
    fd: Int,
    family: Int,
    caps: ChannelCaps,
    local: SocketAddress[IpAddress]
  ): UDPChannelState =
    val mmsgVec = stdlib.calloc(VLEN.toCSize, sizeof[UDPSys.Mmsghdr])
    val iovs = stdlib.calloc(VLEN.toCSize, sizeof[UDPSys.Iovec])
    val names = stdlib.calloc(VLEN.toCSize, SockAddr.storageSize.toCSize)
    val ctls = stdlib.calloc(VLEN.toCSize, PerMsgCtl.toCSize)
    val data = stdlib.calloc(VLEN.toCSize, PerMsgData.toCSize)
    if mmsgVec == null || iovs == null || names == null || ctls == null || data == null then
      throw new OutOfMemoryError("emile: UDPChannel receive buffer allocation failed")
    val state = new UDPChannelState(live, fd, family, caps, local, mmsgVec, iovs, names, ctls, data)
    initRecvVec(state)
    state
  end allocateState

  // ECN reception is always requested (cheap, required by the profile); PKTINFO, GRO, PMTUD, and
  // SO_REUSEPORT are applied per config. GSO / GRO support is probed separately (probeCaps); a failure
  // to apply a requested feature that is core to the channel fails the bind, GRO does not (it is an
  // optimisation).
  private def configureSocket(fd: Int, family: Int, config: ChannelConfig): Either[EmileError.Bind, Unit] =
    boundary:
      if config.reusePort then require(fd, UDPSys.SOL_SOCKET, UDPSys.SO_REUSEPORT, 1)
      if family == UDPSys.AF_INET then
        require(fd, UDPSys.IPPROTO_IP, UDPSys.IP_RECVTOS, 1)
        if config.pktinfo then require(fd, UDPSys.IPPROTO_IP, UDPSys.IP_PKTINFO, 1)
        require(fd, UDPSys.IPPROTO_IP, UDPSys.IP_MTU_DISCOVER, pmtudValue(config.pmtud))
      else
        require(fd, UDPSys.IPPROTO_IPV6, UDPSys.IPV6_RECVTCLASS, 1)
        if config.pktinfo then require(fd, UDPSys.IPPROTO_IPV6, UDPSys.IPV6_RECVPKTINFO, 1)
        require(fd, UDPSys.IPPROTO_IPV6, UDPSys.IPV6_MTU_DISCOVER, pmtudValue(config.pmtud))
        if config.pmtud != PmtudMode.Dont then require(fd, UDPSys.IPPROTO_IPV6, UDPSys.IPV6_DONTFRAG, 1)
      if config.gro then setInt(fd, UDPSys.SOL_UDP, UDPSys.UDP_GRO, 1): Unit
      Right(())

  // A setsockopt that must succeed, breaking the enclosing boundary with a typed Bind error otherwise.
  private def require(fd: Int, level: Int, option: Int, value: Int)(using boundary.Label[Either[EmileError.Bind, Unit]]): Unit =
    if setInt(fd, level, option, value) != 0 then break(Left(BindMapping.fromCode(-libcErrno.errno)))

  private def probeCaps(fd: Int, config: ChannelConfig): ChannelCaps =
    // Detection sets UDP_SEGMENT to 0 (the default), so it leaves per-socket GSO off - sends carry the
    // segment size per datagram via cmsg instead.
    val gso = setInt(fd, UDPSys.SOL_UDP, UDPSys.UDP_SEGMENT, 0) == 0
    // GRO was applied in configureSocket only when requested; probe support directly here.
    val gro = setInt(fd, UDPSys.SOL_UDP, UDPSys.UDP_GRO, if config.gro then 1 else 0) == 0
    ChannelCaps(gso, gro, if gso then kernelMaxGsoSegments else 1)

  private def setInt(fd: Int, level: Int, option: Int, value: Int): Int =
    val cell = stackalloc[CInt]()
    !cell = value
    UDPSys.setsockopt(fd, level, option, cell.asInstanceOf[Ptr[Byte]], sizeof[CInt].toUInt)

  private def getInt(fd: Int, level: Int, option: Int): Either[Int, Int] =
    val cell = stackalloc[CInt]()
    val len = stackalloc[CUnsignedInt]()
    !len = sizeof[CInt].toUInt
    val rc = UDPSys.getsockopt(fd, level, option, cell.asInstanceOf[Ptr[Byte]], len)
    if rc != 0 then Left(-libcErrno.errno) else Right(!cell)

  private def pmtudValue(mode: PmtudMode): Int = mode match
    case PmtudMode.Dont => UDPSys.IP_PMTUDISC_DONT
    case PmtudMode.Want => UDPSys.IP_PMTUDISC_WANT
    case PmtudMode.Do => UDPSys.IP_PMTUDISC_DO
    case PmtudMode.Probe => UDPSys.IP_PMTUDISC_PROBE

  private def readLocalAddress(fd: Int): Either[Int, SocketAddress[IpAddress]] =
    val storage = stackalloc[Byte](SockAddr.storageSize.toCSize)
    val len = stackalloc[CUnsignedInt]()
    !len = SockAddr.storageSize.toUInt
    val rc = UDPSys.getsockname(fd, storage, len)
    if rc != 0 then Left(-libcErrno.errno) else SockAddr.read(storage).toRight(0)

  private def sockaddrLen(address: SocketAddress[IpAddress]): Int =
    address.host.fold(_ => 16, _ => 28)

  // UDP_MAX_SEGMENTS is a kernel-global constant, so it is measured once per process: a self-connected
  // loopback socket accepts a GSO send of up to that many one-byte segments and rejects more with
  // EINVAL. The largest accepted power of two is the cap (the kernel value is a power of two).
  private lazy val kernelMaxGsoSegments: Int = measureMaxGsoSegments()

  private def measureMaxGsoSegments(): Int =
    val fd = UDPSys.socket(UDPSys.AF_INET, UDPSys.SOCK_DGRAM, 0)
    if fd < 0 then 1
    else
      // A sockaddr_in for 127.0.0.1:0: family @0, port 0 @2, address bytes @4.
      val addr = stackalloc[Byte](16.toCSize)
      !addr.asInstanceOf[Ptr[CUnsignedShort]] = UDPSys.AF_INET.toUShort
      addr(4) = 127.toByte; addr(5) = 0.toByte; addr(6) = 0.toByte; addr(7) = 1.toByte
      val bound = UDPSys.bind(fd, addr, 16.toUInt) == 0
      val len = stackalloc[CUnsignedInt](); !len = 16.toUInt
      val named = UDPSys.getsockname(fd, addr, len) == 0
      val connected = named && UDPSys.connect(fd, addr, 16.toUInt) == 0
      val result =
        if bound && connected then
          var candidate = 1024
          var found = 1
          while candidate >= 1 && found == 1 do if gsoSendAccepts(fd, candidate) then found = candidate else candidate = candidate / 2
          found
        else 1
      UDPSys.close(fd): Unit
      result
    end if
  end measureMaxGsoSegments

  // A GSO send of `segments` one-byte segments to the self-connected fd; true unless the kernel rejects
  // it (EINVAL for exceeding UDP_MAX_SEGMENTS).
  private def gsoSendAccepts(fd: Int, segments: Int): Boolean =
    val payload = stdlib.calloc(segments.toCSize, 1.toCSize)
    val iov = stackalloc[UDPSys.Iovec]()
    iov._1 = payload
    iov._2 = segments.toCSize
    val ctl = stackalloc[Byte](24.toCSize)
    writeCmsgU16(ctl, UDPSys.SOL_UDP, UDPSys.UDP_SEGMENT, 1)
    val mh = stackalloc[UDPSys.Msghdr]()
    mh._1 = nullPtr[Byte]; mh._2 = 0.toUInt; mh._3 = iov; mh._4 = 1.toUSize
    mh._5 = ctl; mh._6 = cmsgSpace(2).toCSize; mh._7 = 0
    val mv = stackalloc[UDPSys.Mmsghdr]()
    val hdr = mv.at1
    hdr._1 = mh._1; hdr._2 = mh._2; hdr._3 = mh._3; hdr._4 = mh._4; hdr._5 = mh._5; hdr._6 = mh._6; hdr._7 = mh._7
    val rc = UDPSys.sendmmsg(fd, mv.asInstanceOf[Ptr[Byte]], 1.toUInt, UDPSys.MSG_DONTWAIT)
    stdlib.free(payload)
    rc >= 0
  end gsoSendAccepts

  // Receive-vector layout, set once: each message points at its data slot, name slot, and control
  // slot; only the name and control lengths are reset before each recvmmsg (the kernel shrinks them).
  private def initRecvVec(state: UDPChannelState): Unit =
    val mmsg = state.mmsgVec.asInstanceOf[Ptr[UDPSys.Mmsghdr]]
    val iov = state.iovs.asInstanceOf[Ptr[UDPSys.Iovec]]
    var k = 0
    while k < VLEN do
      val io = iov + k
      io._1 = state.data + k.toLong * PerMsgData
      io._2 = PerMsgData.toCSize
      val h = (mmsg + k).at1
      h._1 = state.names + k.toLong * SockAddr.storageSize
      h._2 = SockAddr.storageSize.toUInt
      h._3 = io
      h._4 = 1.toUSize
      h._5 = state.ctls + k.toLong * PerMsgCtl
      h._6 = PerMsgCtl.toCSize
      h._7 = 0
      k += 1
  end initRecvVec

  private def resetRecvVec(state: UDPChannelState): Unit =
    val mmsg = state.mmsgVec.asInstanceOf[Ptr[UDPSys.Mmsghdr]]
    var k = 0
    while k < VLEN do
      val h = (mmsg + k).at1
      h._2 = SockAddr.storageSize.toUInt
      h._6 = PerMsgCtl.toCSize
      k += 1

  private def consumeChannel[E <: Throwable](channel: UDPChannelState, f: Inbound => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
    withReceiving(channel)(consumeLoop(channel, f))

  private def consumeLoop[E <: Throwable](channel: UDPChannelState, f: Inbound => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
    awaitReadable(channel)
      .flatMap(_ => EffIO.lift(Routing.onOwner(poller(channel))(drainReadable(channel, f))))
      .flatMap(_ => consumeLoop(channel, f))

  // Drains every datagram currently available with one or more recvmmsg calls, delivering each
  // (GRO-split) Inbound to f on the loop thread. Right = drained, re-arm; Left = stop with the error.
  private def drainReadable[E <: Throwable](channel: UDPChannelState, f: Inbound => Either[E, Unit]): Either[EmileError.IO | E, Unit] =
    boundary:
      while true do
        resetRecvVec(channel)
        val got = UDPSys.recvmmsg(channel.fd, channel.mmsgVec, VLEN.toUInt, UDPSys.MSG_DONTWAIT, nullPtr[Byte])
        if got < 0 then
          val err = libcErrno.errno
          if err == posixErrno.EAGAIN || err == posixErrno.EWOULDBLOCK then break(Right(()))
          else break(Left(IOMapping.fromCode(-err)))
        else
          var k = 0
          while k < got do
            deliverMessage(channel, k, f) match
              case Right(()) => ()
              case left => break(left)
            k += 1
          if got < VLEN then break(Right(()))
      Right(())

  private def deliverMessage[E <: Throwable](
    channel: UDPChannelState,
    k: Int,
    f: Inbound => Either[E, Unit]
  ): Either[EmileError.IO | E, Unit] =
    val mmsg = channel.mmsgVec.asInstanceOf[Ptr[UDPSys.Mmsghdr]]
    val len = (mmsg + k)._2.toInt
    val h = (mmsg + k).at1
    SockAddr.read(h._1) match
      case None => Right(()) // non-INET family: not deliverable to an ip4s peer
      case Some(peer) =>
        val parsed = parseControl(h._5, h._6.toLong)
        val dataPtr = channel.data + k.toLong * PerMsgData
        deliverSplit(dataPtr, len, peer, parsed.ecn, parsed.local, parsed.groSegment, f)

  private def deliverSplit[E <: Throwable](
    dataPtr: Ptr[Byte],
    len: Int,
    peer: SocketAddress[IpAddress],
    ecn: ECN,
    local: Option[LocalInfo],
    groSegment: Int,
    f: Inbound => Either[E, Unit]
  ): Either[EmileError.IO | E, Unit] =
    if groSegment > 0 && len > groSegment then
      boundary:
        var off = 0
        while off < len do
          val seg = math.min(groSegment, len - off)
          deliverOne(dataPtr + off, seg, peer, ecn, local, f) match
            case Right(()) => ()
            case left => break(left)
          off += seg
        Right(())
    else deliverOne(dataPtr, len, peer, ecn, local, f)

  private def deliverOne[E <: Throwable](
    ptr: Ptr[Byte],
    len: Int,
    peer: SocketAddress[IpAddress],
    ecn: ECN,
    local: Option[LocalInfo],
    f: Inbound => Either[E, Unit]
  ): Either[EmileError.IO | E, Unit] =
    try f(Inbound(Slice.of(ptr, len), peer, ecn, local))
    catch case t: Throwable => Left(EmileError.IO.Unexpected(t))

  // The parsed ancillary data of one received datagram.
  final private case class Control(ecn: ECN, local: Option[LocalInfo], groSegment: Int)

  private def parseControl(ctl: Ptr[Byte], controllen: Long): Control =
    var ecn = ECN.NotEct
    var local = Option.empty[LocalInfo]
    var groSegment = -1
    if controllen >= 16 then
      var p = ctl
      var rem = controllen
      while rem >= 16 do
        val clen = (!p.asInstanceOf[Ptr[CSize]]).toLong
        val level = !(p + 8).asInstanceOf[Ptr[CInt]]
        val ctype = !(p + 12).asInstanceOf[Ptr[CInt]]
        val data = p + 16
        if level == UDPSys.IPPROTO_IP && ctype == UDPSys.IP_TOS then ecn = ecnOf((!data).toInt & 0x3)
        else if level == UDPSys.IPPROTO_IPV6 && ctype == UDPSys.IPV6_TCLASS then ecn = ecnOf(!data.asInstanceOf[Ptr[CInt]] & 0x3)
        else if level == UDPSys.IPPROTO_IP && ctype == UDPSys.IP_PKTINFO then local = readPktinfoV4(data)
        else if level == UDPSys.IPPROTO_IPV6 && ctype == UDPSys.IPV6_PKTINFO then local = readPktinfoV6(data)
        else if level == UDPSys.SOL_UDP && ctype == UDPSys.UDP_GRO then groSegment = !data.asInstanceOf[Ptr[CInt]]
        val adv = (clen + 7) & ~7L
        if adv <= 0 || adv > rem then rem = 0
        else
          p = p + adv; rem -= adv
    end if
    Control(ecn, local, groSegment)
  end parseControl

  private def ecnOf(codepoint: Int): ECN = ECN.fromOrdinal(codepoint & 0x3)

  // in_pktinfo: ipi_ifindex @0, ipi_addr @8 (the datagram's local destination address).
  private def readPktinfoV4(data: Ptr[Byte]): Option[LocalInfo] =
    val ifindex = !data.asInstanceOf[Ptr[CInt]]
    ipv4FromPtr(data + 8).map(a => LocalInfo(a, ifindex))

  // in6_pktinfo: ipi6_addr @0 (16 bytes), ipi6_ifindex @16.
  private def readPktinfoV6(data: Ptr[Byte]): Option[LocalInfo] =
    val ifindex = !(data + 16).asInstanceOf[Ptr[CInt]]
    ipv6FromPtr(data).map(a => LocalInfo(a, ifindex))

  private def ipv4FromPtr(p: Ptr[Byte]): Option[IpAddress] =
    val b = new Array[Byte](4)
    copyFromPtr(p, b)
    Ipv4Address.fromBytes(b)

  private def ipv6FromPtr(p: Ptr[Byte]): Option[IpAddress] =
    val b = new Array[Byte](16)
    copyFromPtr(p, b)
    Ipv6Address.fromBytes(b)

  private def copyFromPtr(src: Ptr[Byte], dst: Array[Byte]): Unit =
    var i = 0
    while i < dst.length do
      dst(i) = src(i)
      i += 1

  private def copyToPtr(src: Array[Byte], dst: Ptr[Byte]): Unit =
    var i = 0
    while i < src.length do
      dst(i) = src(i)
      i += 1

  private def receiveStream(channel: UDPChannelState): EmStream[EmileError.IO, Inbound] =
    val makeQueue: EmIO[EmileError.IO, BoundedQueue[IO, Either[EmileError.IO, Inbound]]] =
      EffIO.liftF(BoundedQueue[IO, Either[EmileError.IO, Inbound]](ReceiveQueueCapacity))
    Stream.eval(makeQueue).flatMap { queue =>
      val produce: EmIO[EmileError.IO, Unit] =
        consumeChannel[Nothing](
          channel,
          (inbound: Inbound) =>
            queue.unsafeTryOffer(Right(ownedInbound(inbound))): Unit
            Right(())
        ).catchAll(e => EffIO.liftF(queue.offer(Left(e))))
      Stream.repeatEval(EffIO.lift[EmileError.IO, Inbound](queue.take)).concurrently(Stream.eval(produce))
    }

  // A received datagram copied out of the borrowed buffer so it may outlive the callback.
  private def ownedInbound(inbound: Inbound): Inbound =
    inbound.copy(payload = Slice.of(inbound.payload.toArray))

  private def sendBatchOf(channel: UDPChannelState, batch: Seq[Outbound]): EmIO[EmileError.IO, Int] =
    if batch.isEmpty then EffIO.succeed(0)
    else
      EffIO.lift(
        Routing.onOwner(poller(channel)):
          LiveHandle.tryUse[Either[EmileError.IO, Int]](channel.live, closedInt): _ =>
            val (sent, err) = sendmmsgOnce(channel, batch)
            // sent < 0 never happens (sendmmsgOnce clamps); a whole-batch EAGAIN is 0 sent; a
            // first-datagram hard failure surfaces typed, a partial count is re-drivable by the caller.
            if sent > 0 || err == 0 then Right(sent)
            else if err == posixErrno.EAGAIN || err == posixErrno.EWOULDBLOCK then Right(0)
            else Left(IOMapping.fromCode(-err))
      )

  private def sendAllOf(channel: UDPChannelState, batch: Seq[Outbound]): EmIO[EmileError.IO, Unit] =
    def go(remaining: Seq[Outbound]): EmIO[EmileError.IO, Unit] =
      if remaining.isEmpty then EffIO.succeed(())
      else
        EffIO
          .lift(
            Routing.onOwner(poller(channel)):
              LiveHandle.tryUse[Either[EmileError.IO, (Int, Int)]](channel.live, closedProgress)(_ => Right(sendmmsgOnce(channel, remaining)))
          )
          .flatMap { (sent, err) =>
            if sent >= remaining.size then EffIO.succeed(())
            else if sent > 0 then go(remaining.drop(sent))
            // Zero progress: the head could not be sent - surface its errno typed rather than spin.
            else EffIO.fail(IOMapping.fromCode(-err))
          }
    go(batch)
  end sendAllOf

  // One sendmmsg over `batch`, building per-message address and ECN / source / GSO cmsgs; returns the
  // datagram count sent (>= 0) and the errno set when the first datagram failed (else 0).
  private def sendmmsgOnce(channel: UDPChannelState, batch: Seq[Outbound]): (Int, Int) =
    val n = batch.size
    val mmsg = stdlib.calloc(n.toCSize, sizeof[UDPSys.Mmsghdr])
    val iovs = stdlib.calloc(n.toCSize, sizeof[UDPSys.Iovec])
    val names = stdlib.calloc(n.toCSize, SockAddr.storageSize.toCSize)
    val ctls = stdlib.calloc(n.toCSize, SendCtlMax.toCSize)
    if mmsg == null || iovs == null || names == null || ctls == null then
      throw new OutOfMemoryError("emile: UDPChannel send buffer allocation failed")
    val mv = mmsg.asInstanceOf[Ptr[UDPSys.Mmsghdr]]
    val iv = iovs.asInstanceOf[Ptr[UDPSys.Iovec]]
    var i = 0
    val it = batch.iterator
    while it.hasNext do
      val out = it.next()
      val io = iv + i
      io._1 = out.payload.unsafePtr
      io._2 = out.payload.length.toCSize
      val namePtr = names + i.toLong * SockAddr.storageSize
      SockAddr.write(out.to, namePtr)
      val ctlPtr = ctls + i.toLong * SendCtlMax
      val ctlLen = buildSendControl(channel.family, out, ctlPtr)
      val h = (mv + i).at1
      h._1 = namePtr
      h._2 = sockaddrLen(out.to).toUInt
      h._3 = io
      h._4 = 1.toUSize
      h._5 = ctlPtr
      h._6 = ctlLen.toCSize
      h._7 = 0
      i += 1
    end while
    val rc = UDPSys.sendmmsg(channel.fd, mmsg, n.toUInt, UDPSys.MSG_DONTWAIT)
    val err = if rc < 0 then libcErrno.errno else 0
    stdlib.free(mmsg)
    stdlib.free(iovs)
    stdlib.free(names)
    stdlib.free(ctls)
    (if rc < 0 then 0 else rc, err)
  end sendmmsgOnce

  // Writes the ECN (always), source PKTINFO (when set), and GSO (when set) cmsgs for one datagram and
  // returns their total byte length. cmsg families follow the socket, not the destination.
  private def buildSendControl(family: Int, out: Outbound, ctl: Ptr[Byte]): Int =
    var off = 0
    if family == UDPSys.AF_INET then writeCmsgInt(ctl + off, UDPSys.IPPROTO_IP, UDPSys.IP_TOS, out.ecn.ordinal)
    else writeCmsgInt(ctl + off, UDPSys.IPPROTO_IPV6, UDPSys.IPV6_TCLASS, out.ecn.ordinal)
    off += cmsgSpace(4)
    out.source.foreach {
      case v4: Ipv4Address if family == UDPSys.AF_INET =>
        writePktinfoV4(ctl + off, v4.toBytes)
        off += cmsgSpace(12)
      case v6: Ipv6Address if family == UDPSys.AF_INET6 =>
        writePktinfoV6(ctl + off, v6.toBytes)
        off += cmsgSpace(20)
      case _ => () // a source of the other family than the socket cannot be applied
    }
    out.gsoSegment.foreach { seg =>
      writeCmsgU16(ctl + off, UDPSys.SOL_UDP, UDPSys.UDP_SEGMENT, seg)
      off += cmsgSpace(2)
    }
    off
  end buildSendControl

  private def writeCmsgInt(p: Ptr[Byte], level: Int, ctype: Int, value: Int): Unit =
    !p.asInstanceOf[Ptr[CSize]] = cmsgLen(4).toCSize
    !(p + 8).asInstanceOf[Ptr[CInt]] = level
    !(p + 12).asInstanceOf[Ptr[CInt]] = ctype
    !(p + 16).asInstanceOf[Ptr[CInt]] = value

  private def writeCmsgU16(p: Ptr[Byte], level: Int, ctype: Int, value: Int): Unit =
    !p.asInstanceOf[Ptr[CSize]] = cmsgLen(2).toCSize
    !(p + 8).asInstanceOf[Ptr[CInt]] = level
    !(p + 12).asInstanceOf[Ptr[CInt]] = ctype
    !(p + 16).asInstanceOf[Ptr[CUnsignedShort]] = value.toUShort

  // IP_PKTINFO source cmsg: ipi_ifindex @0 = 0 (any), ipi_spec_dst @4 = source, ipi_addr @8 = 0.
  private def writePktinfoV4(p: Ptr[Byte], source: Array[Byte]): Unit =
    !p.asInstanceOf[Ptr[CSize]] = cmsgLen(12).toCSize
    !(p + 8).asInstanceOf[Ptr[CInt]] = UDPSys.IPPROTO_IP
    !(p + 12).asInstanceOf[Ptr[CInt]] = UDPSys.IP_PKTINFO
    !(p + 16).asInstanceOf[Ptr[CInt]] = 0
    copyToPtr(source, p + 20)

  // IPV6_PKTINFO source cmsg: ipi6_addr @0 = source (16 bytes), ipi6_ifindex @16 = 0.
  private def writePktinfoV6(p: Ptr[Byte], source: Array[Byte]): Unit =
    !p.asInstanceOf[Ptr[CSize]] = cmsgLen(20).toCSize
    !(p + 8).asInstanceOf[Ptr[CInt]] = UDPSys.IPPROTO_IPV6
    !(p + 12).asInstanceOf[Ptr[CInt]] = UDPSys.IPV6_PKTINFO
    copyToPtr(source, p + 16)

  // CMSG_LEN(n) = 16 + n; CMSG_SPACE(n) = CMSG_ALIGN(n) + 16, both for the x86_64 8-byte alignment.
  private inline def cmsgLen(dataLen: Int): Int = 16 + dataLen
  private inline def cmsgSpace(dataLen: Int): Int = ((dataLen + 7) & ~7) + 16

  private def setPmtudMode(channel: UDPChannelState, mode: PmtudMode): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(channel)):
        LiveHandle.tryUse(channel.live, closedIo): _ =>
          val (level, option) =
            if channel.family == UDPSys.AF_INET then (UDPSys.IPPROTO_IP, UDPSys.IP_MTU_DISCOVER)
            else (UDPSys.IPPROTO_IPV6, UDPSys.IPV6_MTU_DISCOVER)
          if setInt(channel.fd, level, option, pmtudValue(mode)) != 0 then Left(IOMapping.fromCode(-libcErrno.errno)) else Right(())
    )

  private def pathMtuOf(channel: UDPChannelState): EmIO[EmileError.IO, Int] =
    EffIO.lift(
      Routing.onOwner(poller(channel)):
        LiveHandle.tryUse(channel.live, closedInt): _ =>
          val (level, option) =
            if channel.family == UDPSys.AF_INET then (UDPSys.IPPROTO_IP, UDPSys.IP_MTU) else (UDPSys.IPPROTO_IPV6, UDPSys.IPV6_MTU)
          getInt(channel.fd, level, option).left.map(IOMapping.fromCode)
    )

  private def connectPeer(channel: UDPChannelState, peer: SocketAddress[IpAddress]): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(channel)):
        LiveHandle.tryUse(channel.live, closedIo): _ =>
          val sockaddr = stackalloc[Byte](SockAddr.storageSize.toCSize)
          SockAddr.write(peer, sockaddr)
          if UDPSys.connect(channel.fd, sockaddr, sockaddrLen(peer).toUInt) != 0 then Left(IOMapping.fromCode(-libcErrno.errno))
          else Right(())
    )

  private def disconnectPeer(channel: UDPChannelState): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(channel)):
        LiveHandle.tryUse(channel.live, closedIo): _ =>
          // A connect to an AF_UNSPEC address dissolves the connection; only the family field is read.
          val unspec = stackalloc[Byte](SockAddr.storageSize.toCSize)
          !unspec.asInstanceOf[Ptr[CUnsignedShort]] = UDPSys.AF_UNSPEC.toUShort
          if UDPSys.connect(channel.fd, unspec, sockaddrLen(channel.localAddress).toUInt) != 0 then Left(IOMapping.fromCode(-libcErrno.errno))
          else Right(())
    )

  private def peerAddressOf(channel: UDPChannelState): EmIO[EmileError.IO, SocketAddress[IpAddress]] =
    EffIO.lift(Routing.onOwner(poller(channel))(LiveHandle.tryUse(channel.live, closedPeer)(_ => readPeer(channel.fd))))

  private def readPeer(fd: Int): Either[EmileError.IO, SocketAddress[IpAddress]] =
    val storage = stackalloc[Byte](SockAddr.storageSize.toCSize)
    val len = stackalloc[CUnsignedInt]()
    !len = SockAddr.storageSize.toUInt
    val rc = UDPSys.getpeername(fd, storage, len)
    if rc != 0 then Left(IOMapping.fromCode(-libcErrno.errno))
    else SockAddr.read(storage).toRight(EmileError.IO.Unexpected(new IllegalStateException("emile: unsupported UDP address family")))

  private val PollAwaitEvents = LibUV.UV_READABLE

  // Arm the poll for readability and complete when it fires; the cancellation finaliser stops the poll.
  // The single-receiver guard serialises awaits, so no separate single-waiter flag is needed.
  private def awaitReadable(channel: UDPChannelState): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(channel)):
          LiveHandle.tryUse(channel.live, closedAsync(cb)): handle =>
            CallbackBridge.store(poller(channel), handle, new PollAwait(poller(channel), cb))
            val rc = LibUV.uv_poll_start(handle, PollAwaitEvents, pollCb)
            if rc < 0 then
              CallbackBridge.clear(poller(channel), handle)
              cb(Left(IOMapping.fromCode(rc)))
              None
            else Some(Routing.onOwner(poller(channel))(LiveHandle.tryUse(channel.live, ())(h => stopPoll(poller(channel), h))))
      },
      EmileError.IO.Unexpected(_)
    )

  final private class PollAwait(val poller: LibUVPoller, val cb: Either[Throwable, Unit] => Unit)

  private val pollCb: LibUV.PollCB = (handle: Ptr[Byte], status: CInt, _: CInt) =>
    val holder = CallbackBridge.load[PollAwait](handle)
    stopPoll(holder.poller, handle)
    if status < 0 then holder.cb(Left(IOMapping.fromCode(status))) else holder.cb(Right(()))

  private def stopPoll(poller: LibUVPoller, handle: Ptr[Byte]): Unit =
    LibUV.uv_poll_stop(handle): Unit
    CallbackBridge.clear(poller, handle)

  private def withReceiving[E <: Throwable, A](channel: UDPChannelState)(body: => EmIO[EmileError.IO | E, A]): EmIO[EmileError.IO | E, A] =
    val acquired: EmIO[EmileError.IO | E, Unit] = acquireReceiving(channel)
    acquired.bracket(_ => body)(_ => releaseReceiving(channel))

  private def acquireReceiving(channel: UDPChannelState): EmIO[EmileError.IO, Unit] =
    EffIO.lift(Routing.onOwner(poller(channel))(LiveHandle.tryUse(channel.live, closedIo)(_ => claimReceiving(channel))))

  private def claimReceiving(channel: UDPChannelState): Either[EmileError.IO, Unit] =
    if channel.receiving then Left(EmileError.IO.ConflictingOperation)
    else
      channel.receiving = true
      Right(())

  private def releaseReceiving(channel: UDPChannelState): IO[Unit] =
    Routing.onOwner(poller(channel))(channel.receiving = false)

  private def release(channel: UDPChannelState): IO[Unit] =
    Routing
      .onOwner(poller(channel))(LiveHandle.tryUse(channel.live, ())(handle => LibUV.uv_poll_stop(handle): Unit))
      .flatMap(_ => LiveHandle.closeOnOwner(channel.live))
      .flatMap(_ => IO(freeChannel(channel)))

  private def freeChannel(channel: UDPChannelState): Unit =
    UDPSys.close(channel.fd): Unit
    stdlib.free(channel.mmsgVec)
    stdlib.free(channel.iovs)
    stdlib.free(channel.names)
    stdlib.free(channel.ctls)
    stdlib.free(channel.data)

  private def nullPtr[T]: Ptr[T] = fromRawPtr(Intrinsics.castLongToRawPtr(0L))

  private def poller(channel: UDPChannelState): LibUVPoller = LiveHandle.poller(channel.live)

  private val closedIo: Either[EmileError.IO, Unit] = Left(EmileError.IO.AlreadyClosed)
  private val closedInt: Either[EmileError.IO, Int] = Left(EmileError.IO.AlreadyClosed)
  private val closedProgress: Either[EmileError.IO, (Int, Int)] = Left(EmileError.IO.AlreadyClosed)
  private val closedPeer: Either[EmileError.IO, SocketAddress[IpAddress]] = Left(EmileError.IO.AlreadyClosed)

  private def closedAsync[A](cb: Either[Throwable, A] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.IO.AlreadyClosed))
    Option.empty[IO[Unit]]

  // scalafix:on DisableSyntax

end UDPChannel
