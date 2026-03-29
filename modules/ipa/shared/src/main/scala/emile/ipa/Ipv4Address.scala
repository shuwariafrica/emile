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

import scala.compiletime.error

import boilerplate.*
import boilerplate.nullable.*

/** IPv4 address represented as a 32-bit unsigned integer in host byte order.
  *
  * This is a zero-cost opaque type wrapping `Int`. The four octets are stored as:
  * `(a << 24) | (b << 16) | (c << 8) | d` for address `a.b.c.d`.
  *
  * ==Construction==
  *
  * {{{
  * // From string (runtime validation)
  * val addr: Either[AddressError, Ipv4Address] = Ipv4Address.from("192.168.1.1")
  *
  * // From octets (compile-time validation for literals)
  * val localhost = Ipv4Address.fromOctets(127, 0, 0, 1)
  *
  * // From raw Int (unchecked)
  * val raw = Ipv4Address.fromInt(0x7F000001) // 127.0.0.1
  * }}}
  *
  * ==Well-known Addresses==
  *
  * {{{
  * Ipv4Address.Wildcard  // 0.0.0.0
  * Ipv4Address.Loopback  // 127.0.0.1
  * Ipv4Address.Broadcast // 255.255.255.255
  * }}}
  */
opaque type Ipv4Address = Int

object Ipv4Address extends OpaqueType[Ipv4Address, Int], OpaqueType.Eq[Ipv4Address]:
  type Error = AddressError

  inline def wrap(value: Int): Ipv4Address = value
  inline def unwrap(addr: Ipv4Address): Int = addr
  protected inline def validate(value: Int): Option[AddressError] = None
  inline def apply(inline value: Int): Ipv4Address = value

  given Ordering[Ipv4Address] =
    (x, y) => java.lang.Integer.compareUnsigned(x, y)

  /** Wildcard address 0.0.0.0 - binds to all interfaces. */
  val Wildcard: Ipv4Address = 0

  /** Loopback address 127.0.0.1. */
  val Loopback: Ipv4Address = 0x7f000001

  /** Broadcast address 255.255.255.255. */
  val Broadcast: Ipv4Address = 0xffffffff

  /** Parse an IPv4 address from dotted-decimal string with error details. */
  def from(value: String): Either[AddressError, Ipv4Address] =
    parseIpv4(value)

  private def parseIpv4(value: String | Null): Either[AddressError, Ipv4Address] =
    value.either(AddressError.InvalidIpv4("null", "null input")).flatMap { v =>
      if v.isEmpty then Left(AddressError.InvalidIpv4(v, "empty input"))
      else if v.startsWith(".") || v.endsWith(".") then Left(AddressError.InvalidIpv4(v, "leading or trailing dot"))
      else if v.contains("..") then Left(AddressError.InvalidIpv4(v, "consecutive dots"))
      else
        val parts = v.split('.')
        if parts.length != 4 then Left(AddressError.InvalidIpv4(v, s"expected 4 octets, got ${parts.length}"))
        else
          try
            val octets = parts.map { s =>
              val n = s.toInt
              if n < 0 || n > 255 then throw new IllegalArgumentException(s"octet $n out of range") // scalafix:ok; internal try/catch control flow
              n
            }
            Right(
              ((octets(0) & 0xff) << 24) |
                ((octets(1) & 0xff) << 16) |
                ((octets(2) & 0xff) << 8) |
                (octets(3) & 0xff)
            )
          catch
            case _: NumberFormatException =>
              Left(AddressError.InvalidIpv4(v, "invalid octet format"))
            case e: IllegalArgumentException =>
              Left(AddressError.InvalidIpv4(v, e.getMessage))
        end if
    }

  /** Construct from four octets with compile-time validation for literals.
    *
    * @param a First octet (0-255)
    * @param b Second octet (0-255)
    * @param c Third octet (0-255)
    * @param d Fourth octet (0-255)
    * @return The IPv4 address
    */
  inline def fromOctets(
    inline a: Int,
    inline b: Int,
    inline c: Int,
    inline d: Int
  ): Ipv4Address =
    inline if a < 0 || a > 255 || b < 0 || b > 255 || c < 0 || c > 255 || d < 0 || d > 255 then error("IPv4 octets must be in range 0-255")
    else ((a & 0xff) << 24) | ((b & 0xff) << 16) | ((c & 0xff) << 8) | (d & 0xff)

  /** Construct from four octets at runtime with validation.
    *
    * @param a First octet (0-255)
    * @param b Second octet (0-255)
    * @param c Third octet (0-255)
    * @param d Fourth octet (0-255)
    * @return Either an error or the IPv4 address
    */
  def from(a: Int, b: Int, c: Int, d: Int): Either[AddressError, Ipv4Address] =
    if a < 0 || a > 255 || b < 0 || b > 255 || c < 0 || c > 255 || d < 0 || d > 255 then
      Left(AddressError.InvalidIpv4(s"$a.$b.$c.$d", "octets must be in range 0-255"))
    else Right(((a & 0xff) << 24) | ((b & 0xff) << 16) | ((c & 0xff) << 8) | (d & 0xff))

  /** Construct from raw 32-bit integer (alias for `wrap`). */
  inline def fromInt(value: Int): Ipv4Address = wrap(value)

  /** Construct from byte array (must be exactly 4 bytes, network order).
    *
    * @param bytes The 4-byte array
    * @return Some(Ipv4Address) if valid, None otherwise
    */
  def from(bytes: Array[Byte]): Either[AddressError, Ipv4Address] =
    if bytes.length != 4 then Left(AddressError.InvalidIpv4("<bytes>", s"expected 4 bytes, got ${bytes.length}"))
    else
      Right(
        ((bytes(0) & 0xff) << 24) |
          ((bytes(1) & 0xff) << 16) |
          ((bytes(2) & 0xff) << 8) |
          (bytes(3) & 0xff)
      )

  extension (addr: Ipv4Address)
    /** Get the underlying 32-bit integer. */
    transparent inline def toInt: Int = addr

    /** Get the first octet. */
    transparent inline def octet1: Int = (addr >>> 24) & 0xff

    /** Get the second octet. */
    transparent inline def octet2: Int = (addr >>> 16) & 0xff

    /** Get the third octet. */
    transparent inline def octet3: Int = (addr >>> 8) & 0xff

    /** Get the fourth octet. */
    transparent inline def octet4: Int = addr & 0xff

    /** Convert to network order byte array. */
    def toBytes: Array[Byte] =
      Array(octet1.toByte, octet2.toByte, octet3.toByte, octet4.toByte)

    /** Append dotted-decimal representation to an Appendable. */
    def writeTo[A <: Appendable](out: A): A =
      appendDec(out, octet1)
      out.append('.')
      appendDec(out, octet2)
      out.append('.')
      appendDec(out, octet3)
      out.append('.')
      appendDec(out, octet4)
      out

    /** Dotted-decimal string representation. */
    def show: String =
      val sb = new java.lang.StringBuilder
      writeTo(sb): Unit
      sb.toString

    /** True if this is a loopback address (127.x.x.x). */
    transparent inline def isLoopback: Boolean = octet1 == 127

    /** True if this is a private address (10.x.x.x, 172.16-31.x.x, 192.168.x.x). */
    transparent inline def isPrivate: Boolean =
      octet1 == 10 ||
        (octet1 == 172 && octet2 >= 16 && octet2 <= 31) ||
        (octet1 == 192 && octet2 == 168)

    /** True if this is a link-local address (169.254.x.x). */
    transparent inline def isLinkLocal: Boolean = octet1 == 169 && octet2 == 254

    /** True if this is a multicast address (224-239.x.x.x). */
    transparent inline def isMulticast: Boolean = octet1 >= 224 && octet1 <= 239

    /** True if this is the wildcard address (0.0.0.0). */
    transparent inline def isWildcard: Boolean = addr == 0

    /** True if this is the broadcast address (255.255.255.255). */
    transparent inline def isBroadcast: Boolean = addr == 0xffffffff

    private def appendDec(out: Appendable, value: Int): Unit =
      out.append(java.lang.Integer.toString(value))
      ()
  end extension

end Ipv4Address
