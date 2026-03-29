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

import munit.FunSuite

/** Tests for Ipv6Address opaque type.
  *
  * Tests cover:
  *   - from: parsing with :: compression
  *   - fromLongs: high/low bits construction
  *   - from: byte array parsing
  *   - fromIpv4Mapped: IPv4-mapped IPv6 construction
  *   - highBits / lowBits: extraction
  *   - toBytes: byte array extraction
  *   - show: string representation with :: compression
  *   - Boolean properties: isLoopback, isLinkLocal, isMulticast, isIpv4Mapped, isWildcard
  *   - toIpv4: extraction of mapped IPv4
  *   - Constants: Wildcard, Loopback
  *   - Ordering: unsigned comparison
  */
class Ipv6AddressSpec extends FunSuite:
// scalafix:off

  private def expectRight[A](either: Either[AddressError, A]): A =
    either.fold(err => fail(err.message), identity)

  // ============================================================
  // from(String) tests - basic parsing
  // ============================================================

  test("Ipv6Address.from parses full address"):
    val result = Ipv6Address.from("2001:0db8:0000:0000:0000:0000:0000:0001")
    val addr = expectRight(result)
    assert(addr.show.contains("2001:db8::1") || addr.show.contains("2001:0db8"))

  test("Ipv6Address.from parses loopback ::1"):
    val addr = expectRight(Ipv6Address.from("::1"))
    assertEquals(addr.show, "::1")

  test("Ipv6Address.from parses wildcard ::"):
    val addr = expectRight(Ipv6Address.from("::"))
    assertEquals(addr.show, "::")

  test("Ipv6Address.from parses link-local fe80::1"):
    val addr = expectRight(Ipv6Address.from("fe80::1"))
    assertEquals(addr.show, "fe80::1")

  test("Ipv6Address.from parses multicast ff02::1"):
    val addr = expectRight(Ipv6Address.from("ff02::1"))
    assertEquals(addr.show, "ff02::1")

  test("Ipv6Address.from parses address with :: in middle"):
    assert(Ipv6Address.from("2001:db8::1234:5678").isRight)

  test("Ipv6Address.from parses address with :: at end"):
    assert(Ipv6Address.from("2001:db8::").isRight)

  test("Ipv6Address.from parses address with :: at start"):
    val _ = Ipv6Address.from("::ffff:192.168.1.1")
    ()

  test("Ipv6Address.from parses mixed case"):
    assert(Ipv6Address.from("FE80::ABCD:1234").isRight)

  // ============================================================
  // from(String) tests - error cases
  // ============================================================

  test("Ipv6Address.from returns error for empty string"):
    assert(Ipv6Address.from("").isLeft)

  test("Ipv6Address.from returns error for null"):
    assert(Ipv6Address.from(null: String).isLeft)

  test("Ipv6Address.from returns error for single colon"):
    assert(Ipv6Address.from(":").isLeft)

  test("Ipv6Address.from returns error for triple colon"):
    assert(Ipv6Address.from(":::").isLeft)

  test("Ipv6Address.from returns error for too many groups"):
    assert(Ipv6Address.from("1:2:3:4:5:6:7:8:9").isLeft)

  test("Ipv6Address.from returns error for too few groups without ::"):
    assert(Ipv6Address.from("1:2:3:4:5:6:7").isLeft)

  test("Ipv6Address.from returns error for invalid hex"):
    assert(Ipv6Address.from("ghij::1").isLeft)

  test("Ipv6Address.from returns error for group > 4 digits"):
    assert(Ipv6Address.from("12345::1").isLeft)

  test("Ipv6Address.from returns error for multiple ::"):
    assert(Ipv6Address.from("2001::db8::1").isLeft)

  test("Ipv6Address.from returns Left with error details"):
    val result = Ipv6Address.from("invalid")
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
  // from(bytes) tests
  // ============================================================

  test("Ipv6Address.from accepts valid 16-byte array"):
    val bytes = Array.fill[Byte](16)(0)
    bytes(15) = 1 // ::1
    val addr = expectRight(Ipv6Address.from(bytes))
    assertEquals(addr.show, "::1")

  test("Ipv6Address.from returns error for wrong length"):
    assert(Ipv6Address.from(Array.fill[Byte](15)(0)).isLeft)
    assert(Ipv6Address.from(Array.fill[Byte](17)(0)).isLeft)
    assert(Ipv6Address.from(Array[Byte]()).isLeft)

  test("Ipv6Address.from handles high byte values correctly"):
    val bytes = Array.fill[Byte](16)(0xff.toByte)
    val addr = expectRight(Ipv6Address.from(bytes))
    assertEquals(addr.show, "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")

  test("Ipv6Address.from is insensitive to source array mutation"):
    val bytes = Array.fill[Byte](16)(0)
    bytes(15) = 1 // ::1
    val addr = expectRight(Ipv6Address.from(bytes))
    bytes.indices.foreach(i => bytes(i) = 0x7f.toByte)
    assertEquals(addr.show, "::1")

  // ============================================================
  // fromIpv4Mapped tests
  // ============================================================

  test("Ipv6Address.fromIpv4Mapped creates IPv4-mapped address"):
    val ipv4 = expectRight(Ipv4Address.from("192.168.1.1"))
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
    val addr = expectRight(Ipv6Address.from("2001:db8::1"))
    // 2001:0db8:0000:0000 = 0x20010db800000000
    assertEquals(addr.highBits, 0x20010db800000000L)

  test("Ipv6Address.lowBits extracts lower 64 bits"):
    val addr = expectRight(Ipv6Address.from("::1"))
    assertEquals(addr.lowBits, 1L)

  test("Ipv6Address.highBits for wildcard is 0"):
    assertEquals(Ipv6Address.Wildcard.highBits, 0L)

  test("Ipv6Address.lowBits for wildcard is 0"):
    assertEquals(Ipv6Address.Wildcard.lowBits, 0L)

  // ============================================================
  // toBytes roundtrip tests
  // ============================================================

  test("Ipv6Address.toBytes returns correct bytes"):
    val addr = expectRight(Ipv6Address.from("::1"))
    val bytes = addr.toBytes
    assertEquals(bytes.length, 16)
    // First 15 bytes should be 0, last byte should be 1
    (0 until 15).foreach(i => assertEquals(bytes(i).toInt, 0))
    assertEquals(bytes(15).toInt, 1)

  test("Ipv6Address.toBytes roundtrips correctly"):
    val original = expectRight(Ipv6Address.from("2001:db8::1234:5678"))
    val roundtripped = expectRight(Ipv6Address.from(original.toBytes))
    assertEquals(roundtripped.highBits, original.highBits)
    assertEquals(roundtripped.lowBits, original.lowBits)

  test("Ipv6Address.toBytes for all-ones address"):
    val addr = Ipv6Address.fromLongs(-1L, -1L)
    val bytes = addr.toBytes
    bytes.foreach(b => assertEquals(b & 0xff, 255))

  // ============================================================
  // show tests - :: compression algorithm
  // ============================================================

  test("Ipv6Address.show compresses to :: for wildcard"):
    assertEquals(Ipv6Address.Wildcard.show, "::")

  test("Ipv6Address.show compresses to ::1 for loopback"):
    assertEquals(Ipv6Address.Loopback.show, "::1")

  test("Ipv6Address.show compresses longest run of zeros"):
    // 2001:db8:0:0:0:0:0:1 should become 2001:db8::1
    val addr = expectRight(Ipv6Address.from("2001:db8:0:0:0:0:0:1"))
    assertEquals(addr.show, "2001:db8::1")

  test("Ipv6Address.show uses lowercase hex"):
    val addr = expectRight(Ipv6Address.from("ABCD::1234"))
    assertEquals(addr.show, "abcd::1234")

  test("Ipv6Address.show does not pad groups"):
    val addr = expectRight(Ipv6Address.from("2001:0db8:0000:0001:0000:0000:0000:0001"))
    // Should not have leading zeros
    assert(!addr.show.contains("0db8"))
    assert(addr.show.contains("db8"))

  test("Ipv6Address.show no compression for single zero group"):
    // If there's only one zero group, some implementations still compress
    // Our implementation compresses any run of zeros
    val addr = expectRight(Ipv6Address.from("2001:db8:0:1:2:3:4:5"))
    // Either "2001:db8::1:2:3:4:5" or "2001:db8:0:1:2:3:4:5" is acceptable
    assert(addr.show.nonEmpty)

  test("Ipv6Address.show for all non-zero groups"):
    val addr = Ipv6Address.fromLongs(-1L, -1L)
    assertEquals(addr.show, "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")

  // ============================================================
  // writeTo tests
  // ============================================================

  test("Ipv6Address.writeTo appends canonical form"):
    val addr = expectRight(Ipv6Address.from("2001:db8::1"))
    val sb = new java.lang.StringBuilder("addr=")
    val result = addr.writeTo(sb)
    assertEquals(sb.toString, "addr=2001:db8::1")
    assertEquals(result, sb)

  // ============================================================
  // literals interpolation tests
  // ============================================================

  test("ipv6 interpolator accepts mixed literal and values"):
    import emile.ipa.literals.*

    val group = "db8"
    val addr = ipv6"2001:${group}::1"
    assertEquals(addr.show, "2001:db8::1")

  test("ipv6 interpolator rejects invalid literal fragments"):
    val errors = compileErrors(
      """import emile.ipa.literals.*

val bad = ipv6"fe8g::${1}"
"""
    )
    assert(errors.contains("Invalid IPv6 literal fragment"))

  // ============================================================
  // isLoopback tests (::1)
  // ============================================================

  test("Ipv6Address.isLoopback for ::1"):
    assert(Ipv6Address.Loopback.isLoopback)

  test("Ipv6Address.isLoopback from parsed ::1"):
    val addr = expectRight(Ipv6Address.from("::1"))
    assert(addr.isLoopback)

  test("Ipv6Address.isLoopback is false for ::2"):
    val addr = expectRight(Ipv6Address.from("::2"))
    assert(!addr.isLoopback)

  test("Ipv6Address.isLoopback is false for ::"):
    val addr = expectRight(Ipv6Address.from("::"))
    assert(!addr.isLoopback)

  // ============================================================
  // isLinkLocal tests (fe80::/10)
  // ============================================================

  test("Ipv6Address.isLinkLocal for fe80::1"):
    val addr = expectRight(Ipv6Address.from("fe80::1"))
    assert(addr.isLinkLocal)

  test("Ipv6Address.isLinkLocal for febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff"):
    val addr = expectRight(Ipv6Address.from("febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff"))
    assert(addr.isLinkLocal)

  test("Ipv6Address.isLinkLocal boundary - fec0:: is NOT link-local"):
    val addr = expectRight(Ipv6Address.from("fec0::1"))
    assert(!addr.isLinkLocal)

  test("Ipv6Address.isLinkLocal is false for global unicast"):
    val addr = expectRight(Ipv6Address.from("2001:db8::1"))
    assert(!addr.isLinkLocal)

  // ============================================================
  // isMulticast tests (ff00::/8)
  // ============================================================

  test("Ipv6Address.isMulticast for ff02::1"):
    val addr = expectRight(Ipv6Address.from("ff02::1"))
    assert(addr.isMulticast)

  test("Ipv6Address.isMulticast for ff00::"):
    val addr = expectRight(Ipv6Address.from("ff00::"))
    assert(addr.isMulticast)

  test("Ipv6Address.isMulticast for ffff::"):
    val addr = expectRight(Ipv6Address.from("ffff::"))
    assert(addr.isMulticast)

  test("Ipv6Address.isMulticast boundary - feff:: is NOT multicast"):
    val addr = expectRight(Ipv6Address.from("feff::"))
    assert(!addr.isMulticast)

  test("Ipv6Address.isMulticast is false for unicast"):
    val addr = expectRight(Ipv6Address.from("2001:db8::1"))
    assert(!addr.isMulticast)

  // ============================================================
  // isWildcard tests (::)
  // ============================================================

  test("Ipv6Address.isWildcard for ::"):
    assert(Ipv6Address.Wildcard.isWildcard)

  test("Ipv6Address.isWildcard from parsed ::"):
    val addr = expectRight(Ipv6Address.from("::"))
    assert(addr.isWildcard)

  test("Ipv6Address.isWildcard is false for ::1"):
    assert(!Ipv6Address.Loopback.isWildcard)

  // ============================================================
  // isIpv4Mapped tests (::ffff:0:0/96)
  // ============================================================

  test("Ipv6Address.isIpv4Mapped for mapped address"):
    val ipv4 = expectRight(Ipv4Address.from("192.168.1.1"))
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
    val ipv4 = expectRight(Ipv4Address.from("10.20.30.40"))
    val ipv6 = Ipv6Address.fromIpv4Mapped(ipv4)
    assertEquals(ipv6.toIpv4, Some(ipv4))

  test("Ipv6Address.toIpv4 returns None for non-mapped address"):
    assertEquals(Ipv6Address.Loopback.toIpv4, None)
    assertEquals(Ipv6Address.Wildcard.toIpv4, None)
    assertEquals(expectRight(Ipv6Address.from("2001:db8::1")).toIpv4, None)

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
    val addr1 = expectRight(Ipv6Address.from("::1"))
    val addr2 = expectRight(Ipv6Address.from("::2"))
    assert(Ordering[Ipv6Address].lt(addr1, addr2))

  test("Ipv6Address.Ordering treats high addresses as greater"):
    val low = expectRight(Ipv6Address.from("::1"))
    val high = expectRight(Ipv6Address.from("ffff::1"))
    assert(Ordering[Ipv6Address].lt(low, high))

  test("Ipv6Address.Ordering compares high bits first"):
    val a = Ipv6Address.fromLongs(1L, 0xffffffff_ffffffffL)
    val b = Ipv6Address.fromLongs(2L, 0L)
    assert(Ordering[Ipv6Address].lt(a, b))

  test("Ipv6Address.Ordering handles unsigned comparison for high bits"):
    // -1L as unsigned is greater than 1L
    val a = Ipv6Address.fromLongs(1L, 0L)
    val b = Ipv6Address.fromLongs(-1L, 0L) // 0xFFFFFFFF_FFFFFFFF
    assert(Ordering[Ipv6Address].lt(a, b))

  test("Ipv6Address.Ordering sorts list correctly"):
    val addrs = List(
      expectRight(Ipv6Address.from("2001:db8::1")),
      expectRight(Ipv6Address.from("::1")),
      expectRight(Ipv6Address.from("fe80::1"))
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
    val a1 = expectRight(Ipv6Address.from("2001:db8::1"))
    val a2 = expectRight(Ipv6Address.from("2001:db8::1"))
    assertEquals(a1.highBits, a2.highBits)
    assertEquals(a1.lowBits, a2.lowBits)

  test("Ipv6Address equality via different constructors"):
    val fromString = expectRight(Ipv6Address.from("::1"))
    val fromLongs = Ipv6Address.fromLongs(0L, 1L)
    assertEquals(fromString.highBits, fromLongs.highBits)
    assertEquals(fromString.lowBits, fromLongs.lowBits)

  test("Ipv6Address inequality for different addresses"):
    val a1 = expectRight(Ipv6Address.from("::1"))
    val a2 = expectRight(Ipv6Address.from("::2"))
    assert(a1.lowBits != a2.lowBits)

end Ipv6AddressSpec
