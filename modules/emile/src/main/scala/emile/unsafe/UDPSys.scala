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
package emile.unsafe

import scala.scalanative.unsafe.*

/** The raw POSIX / Linux datagram-socket FFI for `UDPChannel` - the batched raw-fd path libuv's
  * `uv_udp` cannot serve (it exposes no ECN, PKTINFO, GSO, GRO, or PMTUD control). Kept apart from
  * [[LibUV]], which is the libuv surface: these are libc syscalls, resolved at link against the
  * always-linked C runtime. The scatter/gather struct layouts (`iovec`, `msghdr`, `mmsghdr`) are
  * the x86_64 LP64 forms; the socket-option, ancillary-message, and PMTUD constants are the Linux
  * ABI values. The `@extern` bindings are re-exported from [[UDPSysExtern]] and reached as
  * `UDPSys.*`.
  */
private[emile] object UDPSys:

  /** `struct iovec` - a scatter/gather buffer descriptor (`iov_base`, `iov_len`). */
  type Iovec = CStruct2[Ptr[Byte], CSize]

  /** `struct msghdr` (56 bytes): `msg_name`, `msg_namelen` (4B + 4B pad), `msg_iov`, `msg_iovlen`,
    * `msg_control`, `msg_controllen`, `msg_flags` (+4B tail pad). The peer address rides `msg_name`
    * and the ancillary (cmsg) block `msg_control`.
    */
  type Msghdr = CStruct7[Ptr[Byte], CUnsignedInt, Ptr[Iovec], CSize, Ptr[Byte], CSize, CInt]

  /** `struct mmsghdr` (64 bytes): an embedded [[Msghdr]] (`.at1`) then `msg_len` (`._2`), the
    * per-datagram byte count `recvmmsg` / `sendmmsg` write back.
    */
  type Mmsghdr = CStruct2[Msghdr, CUnsignedInt]

  // Address families and datagram socket type; the two flag bits fold non-blocking + close-on-exec
  // into the socket(2) type argument (Linux since 2.6.27). A connect(2) to an AF_UNSPEC address is
  // the standard idiom that dissolves a datagram socket's connection.
  inline val AF_UNSPEC = 0
  inline val AF_INET = 2
  inline val AF_INET6 = 10
  inline val SOCK_DGRAM = 2
  inline val SOCK_NONBLOCK = 0x800
  inline val SOCK_CLOEXEC = 0x80000

  // setsockopt / cmsg levels.
  inline val SOL_SOCKET = 1
  inline val IPPROTO_IP = 0
  inline val IPPROTO_IPV6 = 41
  inline val SOL_UDP = 17

  inline val SO_REUSEPORT = 15

  // IPv4 options (linux/in.h). IP_TOS carries the ECN codepoint in its low two bits; IP_RECVTOS
  // requests it back per datagram; IP_PKTINFO both requests the local address / interface on receive
  // and selects the source on send; IP_MTU_DISCOVER sets the DF / PMTUD policy; IP_MTU reads the
  // connected path MTU.
  inline val IP_TOS = 1
  inline val IP_PKTINFO = 8
  inline val IP_MTU_DISCOVER = 10
  inline val IP_RECVTOS = 13
  inline val IP_MTU = 14

  // IP_MTU_DISCOVER modes: DONT never sets DF, WANT sets DF per route, DO always sets DF and enforces
  // the kernel PMTU (oversized -> EMSGSIZE), PROBE sets DF but ignores the discovered PMTU (RFC 8899
  // probing).
  inline val IP_PMTUDISC_DONT = 0
  inline val IP_PMTUDISC_WANT = 1
  inline val IP_PMTUDISC_DO = 2
  inline val IP_PMTUDISC_PROBE = 3

  // IPv6 equivalents (bits/in.h, linux/in6.h). IPV6_TCLASS is the traffic-class octet (ECN in its low
  // two bits); IPV6_DONTFRAG sets DF; IPV6_MTU_DISCOVER takes the same IP_PMTUDISC_* modes.
  inline val IPV6_MTU_DISCOVER = 23
  inline val IPV6_MTU = 24
  inline val IPV6_RECVPKTINFO = 49
  inline val IPV6_PKTINFO = 50
  inline val IPV6_DONTFRAG = 62
  inline val IPV6_RECVTCLASS = 66
  inline val IPV6_TCLASS = 67

  // UDP offload (linux/udp.h): UDP_SEGMENT sets the send-side GSO segment size (per socket or per
  // sendmsg cmsg); UDP_GRO requests receive-side coalescing with a segment-size cmsg.
  inline val UDP_SEGMENT = 103
  inline val UDP_GRO = 104

  // recvmmsg / sendmmsg flag: do not block when the receive queue is drained (the fd is non-blocking
  // anyway, but the flag makes the batched drain explicit).
  inline val MSG_DONTWAIT = 0x40

  export UDPSysExtern.*

end UDPSys

/** Raw `@extern` libc datagram-socket bindings; reached through [[UDPSys]]. `recvmmsg` / `sendmmsg`
  * are the Linux batched syscalls libuv does not wrap.
  */
@extern
private[unsafe] object UDPSysExtern:

  def socket(domain: CInt, `type`: CInt, protocol: CInt): CInt = extern
  def bind(fd: CInt, addr: Ptr[Byte], addrlen: CUnsignedInt): CInt = extern
  def connect(fd: CInt, addr: Ptr[Byte], addrlen: CUnsignedInt): CInt = extern
  def close(fd: CInt): CInt = extern
  def getsockname(fd: CInt, addr: Ptr[Byte], addrlen: Ptr[CUnsignedInt]): CInt = extern
  def getpeername(fd: CInt, addr: Ptr[Byte], addrlen: Ptr[CUnsignedInt]): CInt = extern
  def setsockopt(fd: CInt, level: CInt, optname: CInt, optval: Ptr[Byte], optlen: CUnsignedInt): CInt = extern
  def getsockopt(fd: CInt, level: CInt, optname: CInt, optval: Ptr[Byte], optlen: Ptr[CUnsignedInt]): CInt = extern

  // A single syscall receives / sends a vector of datagrams, each with its own address and ancillary
  // block. recvmmsg's timeout is NULL here (non-blocking drain). Both write each element's msg_len.
  def recvmmsg(fd: CInt, msgvec: Ptr[Byte], vlen: CUnsignedInt, flags: CInt, timeout: Ptr[Byte]): CInt = extern
  def sendmmsg(fd: CInt, msgvec: Ptr[Byte], vlen: CUnsignedInt, flags: CInt): CInt = extern

end UDPSysExtern
