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

import scala.compiletime.error

/**
 * IPv4 address represented as a 32-bit unsigned integer in host byte order.
 *
 * This is a zero-cost opaque type wrapping `Int`. The four octets are stored
 * as: `(a << 24) | (b << 16) | (c << 8) | d` for address `a.b.c.d`.
 *
 * == Construction ==
 *
 * {{{
 * // From string (runtime validation)
 * val addr: Option[Ipv4Address] = Ipv4Address.fromString("192.168.1.1")
 *
 * // From octets (compile-time validation for literals)
 * val localhost = Ipv4Address.fromOctets(127, 0, 0, 1)
 *
 * // From raw Int (unchecked)
 * val raw = Ipv4Address.fromInt(0x7F000001) // 127.0.0.1
 * }}}
 *
 * == Well-known Addresses ==
 *
 * {{{
 * Ipv4Address.Wildcard  // 0.0.0.0
 * Ipv4Address.Loopback  // 127.0.0.1
 * Ipv4Address.Broadcast // 255.255.255.255
 * }}}
 */
opaque type Ipv4Address = Int

object Ipv4Address:
  given CanEqual[Ipv4Address, Ipv4Address] = CanEqual.derived

  given Ordering[Ipv4Address] =
    // Unsigned comparison
    (x, y) => java.lang.Integer.compareUnsigned(x, y)

  /** Wildcard address 0.0.0.0 - binds to all interfaces. */
  val Wildcard: Ipv4Address = 0

  /** Loopback address 127.0.0.1. */
  val Loopback: Ipv4Address = 0x7f000001

  /** Broadcast address 255.255.255.255. */
  val Broadcast: Ipv4Address = 0xffffffff

  /**
   * Parse an IPv4 address from dotted-decimal string.
   *
   * @param value
   *   The string to parse (e.g., "192.168.1.1")
   * @return
   *   Some(Ipv4Address) if valid, None otherwise
   */
  def fromString(value: String): Option[Ipv4Address] =
    parseIpv4(value).toOption

  /**
   * Parse an IPv4 address from dotted-decimal string with error details.
   *
   * @param value
   *   The string to parse (e.g., "192.168.1.1")
   * @return
   *   Either an error or the parsed address
   */
  def parse(value: String): Either[AddressError, Ipv4Address] =
    parseIpv4(value)

  private def parseIpv4(value: String): Either[AddressError, Ipv4Address] =
    if value == null || value.isEmpty then
      Left(AddressError.InvalidIpv4(value, "empty input"))
    else if value.startsWith(".") || value.endsWith(".") then
      Left(AddressError.InvalidIpv4(value, "leading or trailing dot"))
    else if value.contains("..") then
      Left(AddressError.InvalidIpv4(value, "consecutive dots"))
    else
      val parts = value.split('.')
      if parts.length != 4 then
        Left(AddressError.InvalidIpv4(value, s"expected 4 octets, got ${parts.length}"))
      else
        try
          val octets = parts.map { s =>
            val n = s.toInt
            if n < 0 || n > 255 then
              throw new IllegalArgumentException(s"octet $n out of range")
            n
          }
          Right(
            ((octets(0) & 0xff) << 24) |
              ((octets(1) & 0xff) << 16) |
              ((octets(2) & 0xff) << 8) |
              (octets(3) & 0xff)
          )
        catch
          case e: NumberFormatException =>
            Left(AddressError.InvalidIpv4(value, "invalid octet format"))
          case e: IllegalArgumentException =>
            Left(AddressError.InvalidIpv4(value, e.getMessage.nn))

  /**
   * Construct from four octets with compile-time validation for literals.
   *
   * @param a
   *   First octet (0-255)
   * @param b
   *   Second octet (0-255)
   * @param c
   *   Third octet (0-255)
   * @param d
   *   Fourth octet (0-255)
   * @return
   *   The IPv4 address
   */
  inline def fromOctets(
      inline a: Int,
      inline b: Int,
      inline c: Int,
      inline d: Int
  ): Ipv4Address =
    inline if a < 0 || a > 255 || b < 0 || b > 255 || c < 0 || c > 255 || d < 0 || d > 255 then
      error("IPv4 octets must be in range 0-255")
    else ((a & 0xff) << 24) | ((b & 0xff) << 16) | ((c & 0xff) << 8) | (d & 0xff)

  /**
   * Construct from four octets at runtime with validation.
   *
   * @param a
   *   First octet (0-255)
   * @param b
   *   Second octet (0-255)
   * @param c
   *   Third octet (0-255)
   * @param d
   *   Fourth octet (0-255)
   * @return
   *   Either an error or the IPv4 address
   */
  def fromOctetsRuntime(a: Int, b: Int, c: Int, d: Int): Either[AddressError, Ipv4Address] =
    if a < 0 || a > 255 || b < 0 || b > 255 || c < 0 || c > 255 || d < 0 || d > 255 then
      Left(AddressError.InvalidIpv4(s"$a.$b.$c.$d", "octets must be in range 0-255"))
    else Right(((a & 0xff) << 24) | ((b & 0xff) << 16) | ((c & 0xff) << 8) | (d & 0xff))

  /**
   * Construct from raw 32-bit integer.
   *
   * @param value
   *   The 32-bit address in host byte order
   * @return
   *   The IPv4 address
   */
  inline def fromInt(value: Int): Ipv4Address = value

  /**
   * Construct from byte array (must be exactly 4 bytes, network order).
   *
   * @param bytes
   *   The 4-byte array
   * @return
   *   Some(Ipv4Address) if valid, None otherwise
   */
  def fromBytes(bytes: Array[Byte]): Option[Ipv4Address] =
    if bytes.length != 4 then None
    else
      Some(
        ((bytes(0) & 0xff) << 24) |
          ((bytes(1) & 0xff) << 16) |
          ((bytes(2) & 0xff) << 8) |
          (bytes(3) & 0xff)
      )

  extension (addr: Ipv4Address)
    /** Get the underlying 32-bit integer. */
    inline def toInt: Int = addr

    /** Get the first octet. */
    inline def octet1: Int = (addr >>> 24) & 0xff

    /** Get the second octet. */
    inline def octet2: Int = (addr >>> 16) & 0xff

    /** Get the third octet. */
    inline def octet3: Int = (addr >>> 8) & 0xff

    /** Get the fourth octet. */
    inline def octet4: Int = addr & 0xff

    /** Convert to network order byte array. */
    def toBytes: Array[Byte] =
      Array(octet1.toByte, octet2.toByte, octet3.toByte, octet4.toByte)

    /** Dotted-decimal string representation. */
    def show: String = s"$octet1.$octet2.$octet3.$octet4"

    /** True if this is a loopback address (127.x.x.x). */
    def isLoopback: Boolean = octet1 == 127

    /** True if this is a private address (10.x.x.x, 172.16-31.x.x, 192.168.x.x). */
    def isPrivate: Boolean =
      octet1 == 10 ||
        (octet1 == 172 && octet2 >= 16 && octet2 <= 31) ||
        (octet1 == 192 && octet2 == 168)

    /** True if this is a link-local address (169.254.x.x). */
    def isLinkLocal: Boolean = octet1 == 169 && octet2 == 254

    /** True if this is a multicast address (224-239.x.x.x). */
    def isMulticast: Boolean = octet1 >= 224 && octet1 <= 239

    /** True if this is the wildcard address (0.0.0.0). */
    def isWildcard: Boolean = addr == 0

    /** True if this is the broadcast address (255.255.255.255). */
    def isBroadcast: Boolean = addr == 0xffffffff

end Ipv4Address
