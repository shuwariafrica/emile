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

/**
 * Socket address combining an IP address with a port number.
 *
 * This is the fundamental addressing type for TCP and UDP operations. It
 * supports both IPv4 and IPv6 addresses.
 *
 * == Construction ==
 *
 * {{{
 * // IPv4 socket address
 * val v4 = SocketAddress.v4(Ipv4Address.Loopback, Port(8080))
 *
 * // IPv6 socket address
 * val v6 = SocketAddress.v6(Ipv6Address.Loopback, Port(8080))
 *
 * // Parse from string
 * val parsed = SocketAddress.fromString("127.0.0.1:8080")
 * val parsedV6 = SocketAddress.fromString("[::1]:8080")
 * }}}
 *
 * == Convenience Constructors ==
 *
 * {{{
 * SocketAddress.localhost(Port(8080))   // 127.0.0.1:8080
 * SocketAddress.localhost6(Port(8080))  // [::1]:8080
 * SocketAddress.any(Port(8080))         // 0.0.0.0:8080
 * SocketAddress.any6(Port(8080))        // [::]:8080
 * }}}
 */
enum SocketAddress:
  /**
   * IPv4 socket address.
   *
   * @param address
   *   The IPv4 address
   * @param port
   *   The port number
   */
  case V4(address: Ipv4Address, port: Port)

  /**
   * IPv6 socket address.
   *
   * @param address
   *   The IPv6 address
   * @param port
   *   The port number
   * @param flowInfo
   *   IPv6 flow information (traffic class and flow label)
   * @param scopeId
   *   Scope identifier for link-local addresses
   */
  case V6(address: Ipv6Address, port: Port, flowInfo: FlowInfo, scopeId: ScopeId)

end SocketAddress

object SocketAddress:
  given CanEqual[SocketAddress, SocketAddress] = CanEqual.derived

  /** Wildcard IPv4 address on port 0. */
  val Wildcard: SocketAddress = V4(Ipv4Address.Wildcard, Port.Wildcard)

  /**
   * Create an IPv4 socket address.
   *
   * @param address
   *   The IPv4 address
   * @param port
   *   The port number
   * @return
   *   The socket address
   */
  def v4(address: Ipv4Address, port: Port): SocketAddress = V4(address, port)

  /**
   * Create an IPv6 socket address with default flow info and scope ID.
   *
   * @param address
   *   The IPv6 address
   * @param port
   *   The port number
   * @return
   *   The socket address
   */
  def v6(address: Ipv6Address, port: Port): SocketAddress =
    V6(address, port, FlowInfo.Default, ScopeId.Default)

  /**
   * Create an IPv6 socket address with explicit flow info and scope ID.
   *
   * @param address
   *   The IPv6 address
   * @param port
   *   The port number
   * @param flowInfo
   *   The flow information
   * @param scopeId
   *   The scope identifier
   * @return
   *   The socket address
   */
  def v6(
      address: Ipv6Address,
      port: Port,
      flowInfo: FlowInfo,
      scopeId: ScopeId
  ): SocketAddress = V6(address, port, flowInfo, scopeId)

  /**
   * Parse a socket address from string.
   *
   * Supports formats:
   *   - IPv4: "192.168.1.1:8080"
   *   - IPv6: "[::1]:8080" or "[2001:db8::1]:8080"
   *
   * @param value
   *   The string to parse
   * @return
   *   Either an error or the parsed socket address
   */
  def fromString(value: String): Either[AddressError, SocketAddress] =
    if value == null || value.isEmpty then
      Left(AddressError.InvalidSocketAddress(value, "empty input"))
    else if value.startsWith("[") then parseIpv6SocketAddress(value)
    else parseIpv4SocketAddress(value)

  private def parseIpv4SocketAddress(value: String): Either[AddressError, SocketAddress] =
    val colonIndex = value.lastIndexOf(':')
    if colonIndex < 0 then Left(AddressError.InvalidSocketAddress(value, "missing port separator ':'"))
    else
      val addressPart = value.substring(0, colonIndex).nn
      val portPart    = value.substring(colonIndex + 1).nn

      for
        address <- Ipv4Address.parse(addressPart).left.map { e =>
          AddressError.InvalidSocketAddress(value, e.message)
        }
        port <- Port.fromString(portPart).toRight {
          AddressError.InvalidSocketAddress(value, s"invalid port: $portPart")
        }
      yield V4(address, port)

  private def parseIpv6SocketAddress(value: String): Either[AddressError, SocketAddress] =
    // Format: [ipv6]:port
    val closeBracket = value.indexOf(']')
    if closeBracket < 0 then
      Left(AddressError.InvalidSocketAddress(value, "missing closing bracket ']'"))
    else if closeBracket + 1 >= value.length || value.charAt(closeBracket + 1) != ':' then
      Left(AddressError.InvalidSocketAddress(value, "missing port separator ':' after ']'"))
    else
      val addressPart = value.substring(1, closeBracket).nn
      val portPart    = value.substring(closeBracket + 2).nn

      for
        address <- Ipv6Address.parse(addressPart).left.map { e =>
          AddressError.InvalidSocketAddress(value, e.message)
        }
        port <- Port.fromString(portPart).toRight {
          AddressError.InvalidSocketAddress(value, s"invalid port: $portPart")
        }
      yield V6(address, port, FlowInfo.Default, ScopeId.Default)

  /**
   * Convenient localhost constructor for IPv4.
   *
   * @param port
   *   The port number
   * @return
   *   127.0.0.1:port
   */
  def localhost(port: Port): SocketAddress = V4(Ipv4Address.Loopback, port)

  /**
   * Convenient localhost constructor for IPv6.
   *
   * @param port
   *   The port number
   * @return
   *   [::1]:port
   */
  def localhost6(port: Port): SocketAddress = v6(Ipv6Address.Loopback, port)

  /**
   * Convenient any-interface constructor for IPv4.
   *
   * @param port
   *   The port number
   * @return
   *   0.0.0.0:port
   */
  def any(port: Port): SocketAddress = V4(Ipv4Address.Wildcard, port)

  /**
   * Convenient any-interface constructor for IPv6.
   *
   * @param port
   *   The port number
   * @return
   *   [::]:port
   */
  def any6(port: Port): SocketAddress = v6(Ipv6Address.Wildcard, port)

  extension (addr: SocketAddress)
    /** Get the port. */
    def port: Port = addr match
      case V4(_, p)       => p
      case V6(_, p, _, _) => p

    /** True if this is an IPv4 address. */
    def isV4: Boolean = addr match
      case _: V4 => true
      case _: V6 => false

    /** True if this is an IPv6 address. */
    def isV6: Boolean = addr match
      case _: V4 => false
      case _: V6 => true

    /** Fold over the address type. */
    def fold[A](fv4: (Ipv4Address, Port) => A)(
        fv6: (Ipv6Address, Port, FlowInfo, ScopeId) => A
    ): A =
      addr match
        case V4(a, p)          => fv4(a, p)
        case V6(a, p, fi, sid) => fv6(a, p, fi, sid)

    /** String representation. */
    def show: String = addr match
      case V4(a, p)       => s"${a.show}:${p.value}"
      case V6(a, p, _, _) => s"[${a.show}]:${p.value}"

    /** Get the IPv4 address if this is a V4 socket address. */
    def toIpv4: Option[Ipv4Address] = addr match
      case V4(a, _) => Some(a)
      case _        => None

    /** Get the IPv6 address if this is a V6 socket address. */
    def toIpv6: Option[Ipv6Address] = addr match
      case V6(a, _, _, _) => Some(a)
      case _              => None

    /** Create a new socket address with a different port. */
    def withPort(newPort: Port): SocketAddress = addr match
      case V4(a, _)          => V4(a, newPort)
      case V6(a, _, fi, sid) => V6(a, newPort, fi, sid)

end SocketAddress
