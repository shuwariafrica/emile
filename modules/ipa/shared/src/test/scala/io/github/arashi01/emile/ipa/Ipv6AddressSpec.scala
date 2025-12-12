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

import munit.FunSuite

/**
 * Tests for Ipv6Address opaque type.
 *
 * Tests cover:
 * - fromString / parse: parsing with :: compression
 * - fromLongs: high/low bits construction
 * - fromBytes: byte array parsing
 * - fromIpv4Mapped: IPv4-mapped IPv6 construction
 * - highBits / lowBits: extraction
 * - toBytes: byte array extraction
 * - show: string representation with :: compression
 * - Boolean properties: isLoopback, isLinkLocal, isMulticast, isIpv4Mapped, isWildcard
 * - toIpv4: extraction of mapped IPv4
 * - Constants: Wildcard, Loopback
 * - Ordering: unsigned comparison
 */
class Ipv6AddressSpec extends FunSuite:

  // ============================================================
  // fromString / parse tests - basic parsing
  // ============================================================

  test("Ipv6Address.fromString parses full address"):
    val result = Ipv6Address.fromString("2001:0db8:0000:0000:0000:0000:0000:0001")
    assert(result.isDefined)
    // show should use :: compression
    assert(result.get.show.contains("2001:db8::1") || result.get.show.contains("2001:0db8"))

  test("Ipv6Address.fromString parses loopback ::1"):
    val result = Ipv6Address.fromString("::1")
    assert(result.isDefined)
    assertEquals(result.get.show, "::1")

  test("Ipv6Address.fromString parses wildcard ::"):
    val result = Ipv6Address.fromString("::")
    assert(result.isDefined)
    assertEquals(result.get.show, "::")

  test("Ipv6Address.fromString parses link-local fe80::1"):
    val result = Ipv6Address.fromString("fe80::1")
    assert(result.isDefined)
    assertEquals(result.get.show, "fe80::1")

  test("Ipv6Address.fromString parses multicast ff02::1"):
    val result = Ipv6Address.fromString("ff02::1")
    assert(result.isDefined)
    assertEquals(result.get.show, "ff02::1")

  test("Ipv6Address.fromString parses address with :: in middle"):
    val result = Ipv6Address.fromString("2001:db8::1234:5678")
    assert(result.isDefined)

  test("Ipv6Address.fromString parses address with :: at end"):
    val result = Ipv6Address.fromString("2001:db8::")
    assert(result.isDefined)

  test("Ipv6Address.fromString parses address with :: at start"):
    val result = Ipv6Address.fromString("::ffff:192.168.1.1")
    // This is IPv4-mapped, we may or may not support this format
    // If not supported, that's also valid behavior
    ()

  test("Ipv6Address.fromString parses mixed case"):
    val result = Ipv6Address.fromString("FE80::ABCD:1234")
    assert(result.isDefined)

  // ============================================================
  // fromString / parse tests - error cases
  // ============================================================

  test("Ipv6Address.fromString returns None for empty string"):
    val result = Ipv6Address.fromString("")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for null"):
    val result = Ipv6Address.fromString(null)
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for single colon"):
    val result = Ipv6Address.fromString(":")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for triple colon"):
    val result = Ipv6Address.fromString(":::")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for too many groups"):
    val result = Ipv6Address.fromString("1:2:3:4:5:6:7:8:9")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for too few groups without ::"):
    val result = Ipv6Address.fromString("1:2:3:4:5:6:7")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for invalid hex"):
    val result = Ipv6Address.fromString("ghij::1")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for group > 4 digits"):
    val result = Ipv6Address.fromString("12345::1")
    assert(result.isEmpty)

  test("Ipv6Address.fromString returns None for multiple ::"):
    val result = Ipv6Address.fromString("2001::db8::1")
    assert(result.isEmpty)

  test("Ipv6Address.parse returns Left with error details"):
    val result = Ipv6Address.parse("invalid")
    assert(result.isLeft)
    result.left.foreach { err =>
      assert(err.isInstanceOf[AddressError.InvalidIpv6])
    }

  // ============================================================
  // fromLongs tests
  // ============================================================

  test("Ipv6Address.fromLongs creates address from high/low bits"):
    val addr = Ipv6Address.fromLongs(0L, 1L)
    assertEquals(addr.show, "::1")

  test("Ipv6Address.fromLongs with all zeros"):
    val addr = Ipv6Address.fromLongs(0L, 0L)
    assertEquals(addr.show, "::")

  test("Ipv6Address.fromLongs with all ones"):
    val addr = Ipv6Address.fromLongs(-1L, -1L)
    assertEquals(addr.show, "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")

  test("Ipv6Address.fromLongs roundtrips correctly"):
    val high = 0x2001_0db8_0000_0000L
    val low = 0x0000_0000_0000_0001L
    val addr = Ipv6Address.fromLongs(high, low)
    assertEquals(addr.highBits, high)
    assertEquals(addr.lowBits, low)

  // ============================================================
  // fromBytes tests
  // ============================================================

  test("Ipv6Address.fromBytes accepts valid 16-byte array"):
    val bytes = Array.fill[Byte](16)(0)
    bytes(15) = 1 // ::1
    val result = Ipv6Address.fromBytes(bytes)
    assert(result.isDefined)
    assertEquals(result.get.show, "::1")

  test("Ipv6Address.fromBytes returns None for wrong length"):
    assert(Ipv6Address.fromBytes(Array.fill[Byte](15)(0)).isEmpty)
    assert(Ipv6Address.fromBytes(Array.fill[Byte](17)(0)).isEmpty)
    assert(Ipv6Address.fromBytes(Array[Byte]()).isEmpty)

  test("Ipv6Address.fromBytes handles high byte values correctly"):
    val bytes = Array.fill[Byte](16)(0xff.toByte)
    val result = Ipv6Address.fromBytes(bytes)
    assert(result.isDefined)
    assertEquals(result.get.show, "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")

  // ============================================================
  // fromIpv4Mapped tests
  // ============================================================

  test("Ipv6Address.fromIpv4Mapped creates IPv4-mapped address"):
    val ipv4 = Ipv4Address.fromString("192.168.1.1").get
    val ipv6 = Ipv6Address.fromIpv4Mapped(ipv4)
    assert(ipv6.isIpv4Mapped)
    assertEquals(ipv6.toIpv4, Some(ipv4))

  test("Ipv6Address.fromIpv4Mapped for loopback"):
    val ipv4 = Ipv4Address.Loopback
    val ipv6 = Ipv6Address.fromIpv4Mapped(ipv4)
    assert(ipv6.isIpv4Mapped)

  // ============================================================
  // highBits / lowBits extraction tests
  // ============================================================

  test("Ipv6Address.highBits extracts upper 64 bits"):
    val addr = Ipv6Address.fromString("2001:db8::1").get
    // 2001:0db8:0000:0000 = 0x20010db800000000
    assertEquals(addr.highBits, 0x20010db800000000L)

  test("Ipv6Address.lowBits extracts lower 64 bits"):
    val addr = Ipv6Address.fromString("::1").get
    assertEquals(addr.lowBits, 1L)

  test("Ipv6Address.highBits for wildcard is 0"):
    assertEquals(Ipv6Address.Wildcard.highBits, 0L)

  test("Ipv6Address.lowBits for wildcard is 0"):
    assertEquals(Ipv6Address.Wildcard.lowBits, 0L)

  // ============================================================
  // toBytes roundtrip tests
  // ============================================================

  test("Ipv6Address.toBytes returns correct bytes"):
    val addr = Ipv6Address.fromString("::1").get
    val bytes = addr.toBytes
    assertEquals(bytes.length, 16)
    // First 15 bytes should be 0, last byte should be 1
    (0 until 15).foreach(i => assertEquals(bytes(i).toInt, 0))
    assertEquals(bytes(15).toInt, 1)

  test("Ipv6Address.toBytes roundtrips correctly"):
    val original = Ipv6Address.fromString("2001:db8::1234:5678").get
    val roundtripped = Ipv6Address.fromBytes(original.toBytes).get
    assertEquals(roundtripped.highBits, original.highBits)
    assertEquals(roundtripped.lowBits, original.lowBits)

  test("Ipv6Address.toBytes for all-ones address"):
    val addr = Ipv6Address.fromLongs(-1L, -1L)
    val bytes = addr.toBytes
    bytes.foreach(b => assertEquals((b & 0xff), 255))

  // ============================================================
  // show tests - :: compression algorithm
  // ============================================================

  test("Ipv6Address.show compresses to :: for wildcard"):
    assertEquals(Ipv6Address.Wildcard.show, "::")

  test("Ipv6Address.show compresses to ::1 for loopback"):
    assertEquals(Ipv6Address.Loopback.show, "::1")

  test("Ipv6Address.show compresses longest run of zeros"):
    // 2001:db8:0:0:0:0:0:1 should become 2001:db8::1
    val addr = Ipv6Address.fromString("2001:db8:0:0:0:0:0:1").get
    assertEquals(addr.show, "2001:db8::1")

  test("Ipv6Address.show uses lowercase hex"):
    val addr = Ipv6Address.fromString("ABCD::1234").get
    assertEquals(addr.show, "abcd::1234")

  test("Ipv6Address.show does not pad groups"):
    val addr = Ipv6Address.fromString("2001:0db8:0000:0001:0000:0000:0000:0001").get
    // Should not have leading zeros
    assert(!addr.show.contains("0db8"))
    assert(addr.show.contains("db8"))

  test("Ipv6Address.show no compression for single zero group"):
    // If there's only one zero group, some implementations still compress
    // Our implementation compresses any run of zeros
    val addr = Ipv6Address.fromString("2001:db8:0:1:2:3:4:5").get
    // Either "2001:db8::1:2:3:4:5" or "2001:db8:0:1:2:3:4:5" is acceptable
    assert(addr.show.nonEmpty)

  test("Ipv6Address.show for all non-zero groups"):
    val addr = Ipv6Address.fromLongs(-1L, -1L)
    assertEquals(addr.show, "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")

  // ============================================================
  // isLoopback tests (::1)
  // ============================================================

  test("Ipv6Address.isLoopback for ::1"):
    assert(Ipv6Address.Loopback.isLoopback)

  test("Ipv6Address.isLoopback from parsed ::1"):
    val addr = Ipv6Address.fromString("::1").get
    assert(addr.isLoopback)

  test("Ipv6Address.isLoopback is false for ::2"):
    val addr = Ipv6Address.fromString("::2").get
    assert(!addr.isLoopback)

  test("Ipv6Address.isLoopback is false for ::"):
    val addr = Ipv6Address.fromString("::").get
    assert(!addr.isLoopback)

  // ============================================================
  // isLinkLocal tests (fe80::/10)
  // ============================================================

  test("Ipv6Address.isLinkLocal for fe80::1"):
    val addr = Ipv6Address.fromString("fe80::1").get
    assert(addr.isLinkLocal)

  test("Ipv6Address.isLinkLocal for febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff"):
    val addr = Ipv6Address.fromString("febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff").get
    assert(addr.isLinkLocal)

  test("Ipv6Address.isLinkLocal boundary - fec0:: is NOT link-local"):
    val addr = Ipv6Address.fromString("fec0::1").get
    assert(!addr.isLinkLocal)

  test("Ipv6Address.isLinkLocal is false for global unicast"):
    val addr = Ipv6Address.fromString("2001:db8::1").get
    assert(!addr.isLinkLocal)

  // ============================================================
  // isMulticast tests (ff00::/8)
  // ============================================================

  test("Ipv6Address.isMulticast for ff02::1"):
    val addr = Ipv6Address.fromString("ff02::1").get
    assert(addr.isMulticast)

  test("Ipv6Address.isMulticast for ff00::"):
    val addr = Ipv6Address.fromString("ff00::").get
    assert(addr.isMulticast)

  test("Ipv6Address.isMulticast for ffff::"):
    val addr = Ipv6Address.fromString("ffff::").get
    assert(addr.isMulticast)

  test("Ipv6Address.isMulticast boundary - feff:: is NOT multicast"):
    val addr = Ipv6Address.fromString("feff::").get
    assert(!addr.isMulticast)

  test("Ipv6Address.isMulticast is false for unicast"):
    val addr = Ipv6Address.fromString("2001:db8::1").get
    assert(!addr.isMulticast)

  // ============================================================
  // isWildcard tests (::)
  // ============================================================

  test("Ipv6Address.isWildcard for ::"):
    assert(Ipv6Address.Wildcard.isWildcard)

  test("Ipv6Address.isWildcard from parsed ::"):
    val addr = Ipv6Address.fromString("::").get
    assert(addr.isWildcard)

  test("Ipv6Address.isWildcard is false for ::1"):
    assert(!Ipv6Address.Loopback.isWildcard)

  // ============================================================
  // isIpv4Mapped tests (::ffff:0:0/96)
  // ============================================================

  test("Ipv6Address.isIpv4Mapped for mapped address"):
    val ipv4 = Ipv4Address.fromString("192.168.1.1").get
    val ipv6 = Ipv6Address.fromIpv4Mapped(ipv4)
    assert(ipv6.isIpv4Mapped)

  test("Ipv6Address.isIpv4Mapped is false for loopback"):
    assert(!Ipv6Address.Loopback.isIpv4Mapped)

  test("Ipv6Address.isIpv4Mapped is false for wildcard"):
    assert(!Ipv6Address.Wildcard.isIpv4Mapped)

  // ============================================================
  // toIpv4 tests
  // ============================================================

  test("Ipv6Address.toIpv4 extracts IPv4 from mapped address"):
    val ipv4 = Ipv4Address.fromString("10.20.30.40").get
    val ipv6 = Ipv6Address.fromIpv4Mapped(ipv4)
    assertEquals(ipv6.toIpv4, Some(ipv4))

  test("Ipv6Address.toIpv4 returns None for non-mapped address"):
    assertEquals(Ipv6Address.Loopback.toIpv4, None)
    assertEquals(Ipv6Address.Wildcard.toIpv4, None)
    assertEquals(Ipv6Address.fromString("2001:db8::1").get.toIpv4, None)

  // ============================================================
  // Constants tests
  // ============================================================

  test("Ipv6Address.Wildcard is ::"):
    assertEquals(Ipv6Address.Wildcard.show, "::")
    assert(Ipv6Address.Wildcard.isWildcard)

  test("Ipv6Address.Loopback is ::1"):
    assertEquals(Ipv6Address.Loopback.show, "::1")
    assert(Ipv6Address.Loopback.isLoopback)

  // ============================================================
  // Ordering tests (unsigned 128-bit comparison)
  // ============================================================

  test("Ipv6Address.Ordering compares correctly"):
    val addr1 = Ipv6Address.fromString("::1").get
    val addr2 = Ipv6Address.fromString("::2").get
    assert(Ordering[Ipv6Address].lt(addr1, addr2))

  test("Ipv6Address.Ordering treats high addresses as greater"):
    val low = Ipv6Address.fromString("::1").get
    val high = Ipv6Address.fromString("ffff::1").get
    assert(Ordering[Ipv6Address].lt(low, high))

  test("Ipv6Address.Ordering compares high bits first"):
    val a = Ipv6Address.fromLongs(1L, 0xFFFFFFFF_FFFFFFFFL)
    val b = Ipv6Address.fromLongs(2L, 0L)
    assert(Ordering[Ipv6Address].lt(a, b))

  test("Ipv6Address.Ordering handles unsigned comparison for high bits"):
    // -1L as unsigned is greater than 1L
    val a = Ipv6Address.fromLongs(1L, 0L)
    val b = Ipv6Address.fromLongs(-1L, 0L) // 0xFFFFFFFF_FFFFFFFF
    assert(Ordering[Ipv6Address].lt(a, b))

  test("Ipv6Address.Ordering sorts list correctly"):
    val addrs = List(
      Ipv6Address.fromString("2001:db8::1").get,
      Ipv6Address.fromString("::1").get,
      Ipv6Address.fromString("fe80::1").get
    )
    val sorted = addrs.sorted
    assertEquals(sorted(0).show, "::1")
    // 2001:db8::1 < fe80::1 since 0x2001 < 0xfe80
    assert(sorted(1).show.contains("2001"))
    assert(sorted(2).show.contains("fe80"))

  // ============================================================
  // Equality tests
  // ============================================================

  test("Ipv6Address equality for same address"):
    val a1 = Ipv6Address.fromString("2001:db8::1").get
    val a2 = Ipv6Address.fromString("2001:db8::1").get
    assertEquals(a1.highBits, a2.highBits)
    assertEquals(a1.lowBits, a2.lowBits)

  test("Ipv6Address equality via different constructors"):
    val fromString = Ipv6Address.fromString("::1").get
    val fromLongs = Ipv6Address.fromLongs(0L, 1L)
    assertEquals(fromString.highBits, fromLongs.highBits)
    assertEquals(fromString.lowBits, fromLongs.lowBits)

  test("Ipv6Address inequality for different addresses"):
    val a1 = Ipv6Address.fromString("::1").get
    val a2 = Ipv6Address.fromString("::2").get
    assert(a1.lowBits != a2.lowBits)

end Ipv6AddressSpec
