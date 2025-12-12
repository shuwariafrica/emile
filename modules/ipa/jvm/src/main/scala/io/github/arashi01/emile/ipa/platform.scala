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

import java.net.Inet4Address
import java.net.Inet6Address
import java.net.InetAddress
import java.net.InetSocketAddress

/**
 * JVM platform extensions for IP address and socket address types.
 *
 * These extension methods provide conversion between emile-ipa types and
 * java.net.InetAddress/InetSocketAddress types.
 *
 * == Usage ==
 *
 * {{{
 * import io.github.arashi01.emile.ipa.*
 *
 * val addr = Ipv4Address.Loopback
 * val inet: InetAddress = addr.toInetAddress
 *
 * val sockAddr = SocketAddress.localhost(Port(8080))
 * val inetSock: InetSocketAddress = sockAddr.toInetSocketAddress
 * }}}
 */

extension (addr: Ipv4Address)
  /**
   * Convert to a java.net.Inet4Address.
   *
   * @return
   *   The corresponding Inet4Address
   */
  def toInetAddress: Inet4Address =
    InetAddress.getByAddress(addr.toBytes).nn.asInstanceOf[Inet4Address]

extension (addr: Ipv6Address)
  /**
   * Convert to a java.net.Inet6Address.
   *
   * @return
   *   The corresponding Inet6Address
   */
  def toInetAddress: Inet6Address =
    InetAddress.getByAddress(addr.toBytes).nn.asInstanceOf[Inet6Address]

extension (addr: SocketAddress)
  /**
   * Convert to a java.net.InetSocketAddress.
   *
   * @return
   *   The corresponding InetSocketAddress
   */
  def toInetSocketAddress: InetSocketAddress =
    addr match
      case SocketAddress.V4(ipv4, port) =>
        new InetSocketAddress(ipv4.toInetAddress, port.value)
      case SocketAddress.V6(ipv6, port, _, _) =>
        new InetSocketAddress(ipv6.toInetAddress, port.value)

/**
 * Create an Ipv4Address from a java.net.Inet4Address.
 *
 * @param inet
 *   The Inet4Address to convert
 * @return
 *   The corresponding Ipv4Address
 */
def fromInet4Address(inet: Inet4Address): Ipv4Address =
  val bytes = inet.getAddress.nn
  Ipv4Address.fromBytes(bytes).get

/**
 * Create an Ipv6Address from a java.net.Inet6Address.
 *
 * @param inet
 *   The Inet6Address to convert
 * @return
 *   The corresponding Ipv6Address
 */
def fromInet6Address(inet: Inet6Address): Ipv6Address =
  val bytes = inet.getAddress.nn
  Ipv6Address.fromBytes(bytes).get

/**
 * Create a SocketAddress from a java.net.InetSocketAddress.
 *
 * @param inetSock
 *   The InetSocketAddress to convert
 * @return
 *   Either an error or the corresponding SocketAddress
 */
def fromInetSocketAddress(inetSock: InetSocketAddress): Either[AddressError, SocketAddress] =
  val port = Port.fromInt(inetSock.getPort)
  port.map { p =>
    inetSock.getAddress.nn match
      case inet4: Inet4Address =>
        SocketAddress.v4(fromInet4Address(inet4), p)
      case inet6: Inet6Address =>
        SocketAddress.v6(fromInet6Address(inet6), p)
  }
