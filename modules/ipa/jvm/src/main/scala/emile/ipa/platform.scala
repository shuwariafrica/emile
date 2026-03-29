/*
 * Copyright 2025, 2026 Ali Rashid.
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
package emile.ipa

// scalafix:off DisableSyntax.throw, DisableSyntax.asInstanceOf; JVM InetAddress interop requires these

import java.net.Inet4Address
import java.net.Inet6Address
import java.net.InetAddress
import java.net.InetSocketAddress

import boilerplate.nullable.*

private inline def expectRight[A](either: Either[AddressError, A]): A =
  either.fold(err => throw new IllegalArgumentException(err.message), identity)

/** JVM platform extensions for IP address and socket address types.
  *
  * These extension methods provide conversion between emile-ipa types and
  * java.net.InetAddress/InetSocketAddress types.
  *
  * ==Usage==
  *
  * {{{
  * import emile.ipa.*
  *
  * val addr = Ipv4Address.Loopback
  * val inet: InetAddress = addr.toInetAddress
  *
  * val sockAddr = SocketAddress.localhost(Port(8080))
  * val inetSock: InetSocketAddress = sockAddr.toInetSocketAddress
  * }}}
  */

extension (addr: Ipv4Address)
  /** Convert to a java.net.Inet4Address.
    *
    * @return The corresponding Inet4Address
    */
  def toInetAddress: Inet4Address =
    InetAddress
      .getByAddress(addr.toBytes)
      .option
      .map(_.asInstanceOf[Inet4Address])
      .getOrElse(throw new IllegalStateException("Failed to create Inet4Address"))
end extension

extension (addr: Ipv6Address)
  /** Convert to a java.net.Inet6Address.
    *
    * @return The corresponding Inet6Address
    */
  def toInetAddress: Inet6Address =
    InetAddress
      .getByAddress(addr.toBytes)
      .option
      .map(_.asInstanceOf[Inet6Address])
      .getOrElse(throw new IllegalStateException("Failed to create Inet6Address"))
end extension

extension (addr: SocketAddress)
  /** Convert to a java.net.InetSocketAddress.
    *
    * @return The corresponding InetSocketAddress
    */
  def toInetSocketAddress: InetSocketAddress =
    addr match
      case SocketAddress.V4(ipv4, port) =>
        new InetSocketAddress(ipv4.toInetAddress, port.value)
      case SocketAddress.V6(ipv6, port, _, _) =>
        new InetSocketAddress(ipv6.toInetAddress, port.value)
end extension

/** Create an Ipv4Address from a java.net.Inet4Address.
  *
  * @param inet The Inet4Address to convert
  * @return The corresponding Ipv4Address
  */
def fromInet4Address(inet: Inet4Address | Null): Ipv4Address =
  inet.option
    .flatMap(i => i.getAddress.option)
    .flatMap(bytes => Ipv4Address.from(bytes).toOption)
    .getOrElse(throw new IllegalArgumentException("Invalid Inet4Address"))

/** Create an Ipv6Address from a java.net.Inet6Address.
  *
  * @param inet The Inet6Address to convert
  * @return The corresponding Ipv6Address
  */
def fromInet6Address(inet: Inet6Address | Null): Ipv6Address =
  inet.option
    .flatMap(i => i.getAddress.option)
    .flatMap(bytes => Ipv6Address.from(bytes).toOption)
    .getOrElse(throw new IllegalArgumentException("Invalid Inet6Address"))

/** Create a SocketAddress from a java.net.InetSocketAddress.
  *
  * @param inetSock The InetSocketAddress to convert
  * @return Either an error or the corresponding SocketAddress
  */
def fromInetSocketAddress(inetSock: InetSocketAddress | Null): Either[AddressError, SocketAddress] =
  inetSock.option
    .map { sock =>
      val port = Port.from(sock.getPort)
      port.flatMap { p =>
        sock.getAddress.option match
          case Some(inet4: Inet4Address) =>
            Right(SocketAddress.v4(fromInet4Address(inet4), p))
          case Some(inet6: Inet6Address) =>
            Right(SocketAddress.v6(fromInet6Address(inet6), p))
          case _ =>
            Left(AddressError.InvalidSocketAddress("", "Invalid InetAddress"))
      }
    }
    .getOrElse(Left(AddressError.InvalidSocketAddress("null", "null input")))
