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
 * IPv6 address represented as two 64-bit integers.
 *
 * This is a zero-cost opaque type wrapping `(Long, Long)`. The high Long
 * contains the first 64 bits, the low Long contains the last 64 bits.
 *
 * == Construction ==
 *
 * {{{
 * // From string (runtime validation)
 * val addr: Option[Ipv6Address] = Ipv6Address.fromString("::1")
 * val full: Option[Ipv6Address] = Ipv6Address.fromString("2001:db8::1")
 *
 * // From raw Longs (unchecked)
 * val raw = Ipv6Address.fromLongs(highBits, lowBits)
 * }}}
 *
 * == Well-known Addresses ==
 *
 * {{{
 * Ipv6Address.Wildcard // ::
 * Ipv6Address.Loopback // ::1
 * }}}
 */
opaque type Ipv6Address = (Long, Long)

object Ipv6Address:
  given CanEqual[Ipv6Address, Ipv6Address] = CanEqual.derived

  given Ordering[Ipv6Address] =
    // Unsigned comparison of high then low
    (x, y) =>
      val cmpHigh = java.lang.Long.compareUnsigned(x._1, y._1)
      if cmpHigh != 0 then cmpHigh
      else java.lang.Long.compareUnsigned(x._2, y._2)

  /** Wildcard address :: (all zeros). */
  val Wildcard: Ipv6Address = (0L, 0L)

  /** Loopback address ::1. */
  val Loopback: Ipv6Address = (0L, 1L)

  /**
   * Parse an IPv6 address from string.
   *
   * Supports standard IPv6 notation including :: compression.
   *
   * @param value
   *   The string to parse (e.g., "::1", "2001:db8::1")
   * @return
   *   Some(Ipv6Address) if valid, None otherwise
   */
  def fromString(value: String): Option[Ipv6Address] =
    parse(value).toOption

  /**
   * Parse an IPv6 address from string with error details.
   *
   * @param value
   *   The string to parse
   * @return
   *   Either an error or the parsed address
   */
  def parse(value: String): Either[AddressError, Ipv6Address] =
    parseIpv6(value)

  private def parseIpv6(value: String): Either[AddressError, Ipv6Address] =
    if value == null || value.isEmpty then Left(AddressError.InvalidIpv6(value, "empty input"))
    else
      try
        parseIpv6Groups(value).flatMap { groups =>
          // Validate all groups are in range
          val invalidGroup = groups.find(g => g < 0 || g > 0xffff)
          invalidGroup match
            case Some(g) =>
              Left(AddressError.InvalidIpv6(value, s"group value $g out of range"))
            case None =>
              // Combine into two Longs
              val high =
                (groups(0).toLong << 48) |
                  (groups(1).toLong << 32) |
                  (groups(2).toLong << 16) |
                  groups(3).toLong
              val low =
                (groups(4).toLong << 48) |
                  (groups(5).toLong << 32) |
                  (groups(6).toLong << 16) |
                  groups(7).toLong
              Right((high, low))
        }
      catch
        case _: NumberFormatException =>
          Left(AddressError.InvalidIpv6(value, "invalid hexadecimal group"))

  private def parseIpv6Groups(value: String): Either[AddressError, Array[Int]] =
    // Check for invalid patterns first
    if value.contains(":::") then
      return Left(AddressError.InvalidIpv6(value, "triple colon not allowed"))
    
    val doubleColonIndex = value.indexOf("::")
    
    // Check for multiple :: occurrences
    if doubleColonIndex >= 0 && value.indexOf("::", doubleColonIndex + 2) >= 0 then
      return Left(AddressError.InvalidIpv6(value, "multiple :: not allowed"))
    
    if doubleColonIndex < 0 then
      // No compression - must have exactly 8 groups
      val parts = value.split(':')
      if parts.length != 8 then
        Left(AddressError.InvalidIpv6(value, s"expected 8 groups, got ${parts.length}"))
      else 
        // Check for empty groups
        if parts.exists(_.isEmpty) then
          Left(AddressError.InvalidIpv6(value, "empty group not allowed without ::"))
        else
          Right(parts.map(p => java.lang.Integer.parseUnsignedInt(p, 16)))
    else
      // Has compression
      val prefix =
        if doubleColonIndex == 0 then Array.empty[String]
        else value.substring(0, doubleColonIndex).split(':').filter(_.nonEmpty)
      val suffix =
        if doubleColonIndex == value.length - 2 then Array.empty[String]
        else value.substring(doubleColonIndex + 2).split(':').filter(_.nonEmpty)

      val prefixNums = prefix.map(p => java.lang.Integer.parseUnsignedInt(p, 16))
      val suffixNums = suffix.map(p => java.lang.Integer.parseUnsignedInt(p, 16))

      val totalGroups = prefixNums.length + suffixNums.length
      if totalGroups > 7 then Left(AddressError.InvalidIpv6(value, "too many groups with ::"))
      else
        val zeroCount = 8 - totalGroups
        Right(prefixNums ++ Array.fill(zeroCount)(0) ++ suffixNums)

  /**
   * Construct from two 64-bit integers.
   *
   * @param high
   *   The high 64 bits
   * @param low
   *   The low 64 bits
   * @return
   *   The IPv6 address
   */
  inline def fromLongs(high: Long, low: Long): Ipv6Address = (high, low)

  /**
   * Construct from byte array (must be exactly 16 bytes, network order).
   *
   * @param bytes
   *   The 16-byte array
   * @return
   *   Some(Ipv6Address) if valid, None otherwise
   */
  def fromBytes(bytes: Array[Byte]): Option[Ipv6Address] =
    if bytes.length != 16 then None
    else
      var high = 0L
      var low  = 0L
      for i <- 0 until 8 do high = (high << 8) | (bytes(i) & 0xff)
      for i <- 8 until 16 do low = (low << 8) | (bytes(i) & 0xff)
      Some((high, low))

  /**
   * Create an IPv4-mapped IPv6 address (::ffff:a.b.c.d).
   *
   * @param ipv4
   *   The IPv4 address to map
   * @return
   *   The IPv4-mapped IPv6 address
   */
  def fromIpv4Mapped(ipv4: Ipv4Address): Ipv6Address =
    (0L, 0xffffL << 32 | (ipv4.toInt.toLong & 0xffffffffL))

  extension (addr: Ipv6Address)
    /** Get the high 64 bits. */
    inline def highBits: Long = addr._1

    /** Get the low 64 bits. */
    inline def lowBits: Long = addr._2

    /** Convert to network order byte array. */
    def toBytes: Array[Byte] =
      val bytes = new Array[Byte](16)
      var h     = addr._1
      var l     = addr._2
      for i <- 7 to 0 by -1 do
        bytes(i) = (h & 0xff).toByte
        h = h >>> 8
      for i <- 15 to 8 by -1 do
        bytes(i) = (l & 0xff).toByte
        l = l >>> 8
      bytes

    /**
     * RFC 5952 canonical string representation.
     *
     * Uses :: compression for the longest run of zeros, lowercase hex.
     */
    def show: String =
      // Extract 8 groups
      val groups = new Array[Int](8)
      groups(0) = ((addr._1 >>> 48) & 0xffff).toInt
      groups(1) = ((addr._1 >>> 32) & 0xffff).toInt
      groups(2) = ((addr._1 >>> 16) & 0xffff).toInt
      groups(3) = (addr._1 & 0xffff).toInt
      groups(4) = ((addr._2 >>> 48) & 0xffff).toInt
      groups(5) = ((addr._2 >>> 32) & 0xffff).toInt
      groups(6) = ((addr._2 >>> 16) & 0xffff).toInt
      groups(7) = (addr._2 & 0xffff).toInt

      // Find longest run of zeros for :: compression
      var bestStart  = -1
      var bestLength = 0
      var runStart   = -1
      var runLength  = 0

      for i <- 0 until 8 do
        if groups(i) == 0 then
          if runStart < 0 then runStart = i
          runLength += 1
        else
          if runLength > bestLength && runLength > 1 then
            bestStart = runStart
            bestLength = runLength
          runStart = -1
          runLength = 0

      // Check final run
      if runLength > bestLength && runLength > 1 then
        bestStart = runStart
        bestLength = runLength

      // Build string
      val sb = new StringBuilder
      var i  = 0
      var afterCompression = false
      while i < 8 do
        if i == bestStart then
          sb.append("::")
          i += bestLength
          afterCompression = true
        else
          if i > 0 && !afterCompression then sb.append(':')
          afterCompression = false
          sb.append(java.lang.Integer.toHexString(groups(i)))
          i += 1
      sb.toString

    /** True if this is the loopback address (::1). */
    def isLoopback: Boolean = addr._1 == 0L && addr._2 == 1L

    /** True if this is a link-local address (fe80::/10). */
    def isLinkLocal: Boolean = (addr._1 >>> 54) == 0x3fa // fe80 >> 6 = 0x3FA

    /** True if this is a multicast address (ff00::/8). */
    def isMulticast: Boolean = (addr._1 >>> 56) == 0xff

    /** True if this is an IPv4-mapped IPv6 address (::ffff:0:0/96). */
    def isIpv4Mapped: Boolean =
      addr._1 == 0L && (addr._2 >>> 32) == 0xffffL

    /** True if this is the wildcard address (::). */
    def isWildcard: Boolean = addr._1 == 0L && addr._2 == 0L

    /** Extract the IPv4 address if this is an IPv4-mapped address. */
    def toIpv4: Option[Ipv4Address] =
      if isIpv4Mapped then Some(Ipv4Address.fromInt((addr._2 & 0xffffffffL).toInt))
      else None

end Ipv6Address
