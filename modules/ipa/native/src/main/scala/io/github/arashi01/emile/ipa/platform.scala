/*
 * Copyright 2025 the original author(s).
 *
 * Licensed under the MIT License (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://opensource.org/licenses/MIT
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.arashi01.emile.ipa

import scala.scalanative.posix.arpa.inet.*
import scala.scalanative.posix.netinet.in.*
import scala.scalanative.posix.netinet.inOps.*
import scala.scalanative.posix.sys.socket.*
import scala.scalanative.posix.sys.socketOps.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/**
 * Native platform extensions for IP address and socket address types.
 *
 * These extension methods provide zero-copy conversion between emile-ipa types
 * and native sockaddr structures used by POSIX networking APIs.
 *
 * == Usage ==
 *
 * {{{
 * import io.github.arashi01.emile.ipa.*
 *
 * Zone.acquire { implicit z =>
 *   val addr = SocketAddress.v4(Ipv4Address.Loopback, Port(8080))
 *   val sockaddr: Ptr[sockaddr] = addr.toSockAddr
 * }
 * }}}
 */

// Offset of sin6_addr within sockaddr_in6 structure
// This is platform-dependent but typically:
// - sin6_family: 1-2 bytes
// - sin6_port: 2 bytes
// - sin6_flowinfo: 4 bytes
// Total offset to sin6_addr: 8 bytes (with padding)
private inline val SIN6_ADDR_OFFSET = 8

extension (addr: Ipv4Address)
  /**
   * Convert to network byte order (big-endian).
   *
   * @return
   *   The address in network byte order
   */
  inline def toNetworkOrder: CUnsignedInt =
    htonl(addr.toInt.toUInt)

  /**
   * Fill a sockaddr_in structure with this address.
   *
   * @param sockaddr
   *   Pointer to a sockaddr_in structure to fill
   * @param port
   *   The port number
   */
  def fillSockAddrIn(sockaddr: Ptr[sockaddr_in], port: Port): Unit =
    sockaddr.sin_family = AF_INET.toUShort
    sockaddr.sin_port = htons(port.value.toUShort)
    sockaddr.sin_addr.s_addr = htonl(addr.toInt.toUInt)

extension (addr: Ipv6Address)
  /**
   * Fill a 16-byte buffer with the address in network order.
   *
   * @param buf
   *   Pointer to a 16-byte buffer
   */
  def fillNetworkOrder(buf: Ptr[Byte]): Unit =
    val h = addr.highBits
    val l = addr.lowBits
    // Big-endian: write high bits first
    var i = 0
    while i < 8 do
      buf(i) = ((h >>> (56 - i * 8)) & 0xff).toByte
      i += 1
    i = 0
    while i < 8 do
      buf(8 + i) = ((l >>> (56 - i * 8)) & 0xff).toByte
      i += 1

  /**
   * Fill a sockaddr_in6 structure with this address.
   *
   * @param sockaddr
   *   Pointer to a sockaddr_in6 structure to fill
   * @param port
   *   The port number
   * @param flowInfo
   *   IPv6 flow information
   * @param scopeId
   *   IPv6 scope identifier
   */
  def fillSockAddrIn6(
      sockaddr: Ptr[sockaddr_in6],
      port: Port,
      flowInfo: FlowInfo,
      scopeId: ScopeId
  ): Unit =
    sockaddr.sin6_family = AF_INET6.toUShort
    sockaddr.sin6_port = htons(port.value.toUShort)
    sockaddr.sin6_flowinfo = htonl(flowInfo.value.toUInt)
    sockaddr.sin6_scope_id = htonl(scopeId.value.toUInt)
    // Write the 16-byte address to sin6_addr field via raw pointer access
    val addrPtr = sockaddr.asInstanceOf[Ptr[Byte]] + SIN6_ADDR_OFFSET
    fillNetworkOrder(addrPtr)

extension (addr: SocketAddress)
  /**
   * Allocate and fill a sockaddr structure for this address.
   *
   * Must be called within a Zone as memory is allocated.
   *
   * @return
   *   Pointer to the allocated sockaddr structure (sockaddr_in or
   *   sockaddr_in6)
   */
  def toSockAddr(using Zone): Ptr[sockaddr] =
    addr match
      case SocketAddress.V4(ipv4, port) =>
        val sockaddr = alloc[sockaddr_in]()
        ipv4.fillSockAddrIn(sockaddr, port)
        sockaddr.asInstanceOf[Ptr[sockaddr]]

      case SocketAddress.V6(ipv6, port, flowInfo, scopeId) =>
        val sockaddr = alloc[sockaddr_in6]()
        ipv6.fillSockAddrIn6(sockaddr, port, flowInfo, scopeId)
        sockaddr.asInstanceOf[Ptr[sockaddr]]

  /**
   * Get the size of the sockaddr structure for this address type.
   *
   * @return
   *   Size in bytes (16 for IPv4, 28 for IPv6)
   */
  inline def sockAddrSize: CSize = addr match
    case _: SocketAddress.V4 => sizeof[sockaddr_in]
    case _: SocketAddress.V6 => sizeof[sockaddr_in6]

/**
 * Parse a SocketAddress from a sockaddr structure.
 *
 * @param sockaddr
 *   Pointer to a sockaddr structure
 * @return
 *   Either an error or the parsed SocketAddress
 */
def fromSockAddr(sockaddr: Ptr[sockaddr]): Either[AddressError, SocketAddress] =
  val family = sockaddr.sa_family.toInt
  if family == AF_INET then
    val sin     = sockaddr.asInstanceOf[Ptr[sockaddr_in]]
    val addrInt = ntohl(sin.sin_addr.s_addr).toInt
    val portInt = ntohs(sin.sin_port).toInt
    Port.fromInt(portInt) match
      case Right(port) =>
        Right(SocketAddress.v4(Ipv4Address.fromInt(addrInt), port))
      case Left(err) =>
        Left(err)
  else if family == AF_INET6 then
    val sin6 = sockaddr.asInstanceOf[Ptr[sockaddr_in6]]
    // Read the 16-byte address via raw pointer access
    val addrPtr = sin6.asInstanceOf[Ptr[Byte]] + SIN6_ADDR_OFFSET
    var high    = 0L
    var low     = 0L
    var i       = 0
    while i < 8 do
      high = (high << 8) | (addrPtr(i) & 0xff)
      i += 1
    i = 0
    while i < 8 do
      low = (low << 8) | (addrPtr(8 + i) & 0xff)
      i += 1
    val portInt  = ntohs(sin6.sin6_port).toInt
    val flowInfo = FlowInfo(ntohl(sin6.sin6_flowinfo).toInt)
    val scopeId  = ScopeId(ntohl(sin6.sin6_scope_id).toInt)
    Port.fromInt(portInt) match
      case Right(port) =>
        Right(SocketAddress.v6(Ipv6Address.fromLongs(high, low), port, flowInfo, scopeId))
      case Left(err) =>
        Left(err)
  else Left(AddressError.InvalidSocketAddress("", s"unknown address family: $family"))
