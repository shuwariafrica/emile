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
 * Tests for Ipv4Address opaque type.
 *
 * Tests cover:
 * - fromString / parse: string parsing with various formats
 * - fromOctetsRuntime: runtime octet validation
 * - fromBytes: byte array parsing
 * - fromInt: raw integer construction
 * - octet1-4: byte extraction
 * - toBytes: byte array extraction
 * - toInt: integer extraction
 * - show: string representation
 * - Boolean properties: isLoopback, isPrivate, isLinkLocal, isMulticast, isWildcard, isBroadcast
 * - Constants: Wildcard, Loopback, Broadcast
 * - Ordering: unsigned comparison
 */
class Ipv4AddressSpec extends FunSuite:

  // ============================================================
  // fromString / parse tests
  // ============================================================

  test("Ipv4Address.fromString parses valid address"):
    val result = Ipv4Address.fromString("192.168.1.1")
    assert(result.isDefined)
    assertEquals(result.get.show, "192.168.1.1")

  test("Ipv4Address.fromString parses loopback"):
    val result = Ipv4Address.fromString("127.0.0.1")
    assert(result.isDefined)
    assertEquals(result.get.show, "127.0.0.1")

  test("Ipv4Address.fromString parses wildcard"):
    val result = Ipv4Address.fromString("0.0.0.0")
    assert(result.isDefined)
    assertEquals(result.get.show, "0.0.0.0")

  test("Ipv4Address.fromString parses broadcast"):
    val result = Ipv4Address.fromString("255.255.255.255")
    assert(result.isDefined)
    assertEquals(result.get.show, "255.255.255.255")

  test("Ipv4Address.fromString returns None for empty string"):
    val result = Ipv4Address.fromString("")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for whitespace"):
    val result = Ipv4Address.fromString("   ")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for null"):
    val result = Ipv4Address.fromString(null)
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for too few octets"):
    val result = Ipv4Address.fromString("192.168.1")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for too many octets"):
    val result = Ipv4Address.fromString("192.168.1.1.1")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for octet > 255"):
    val result = Ipv4Address.fromString("192.168.1.256")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for negative octet"):
    val result = Ipv4Address.fromString("192.168.1.-1")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for non-numeric octet"):
    val result = Ipv4Address.fromString("192.168.1.abc")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for trailing dot"):
    val result = Ipv4Address.fromString("192.168.1.1.")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for leading dot"):
    val result = Ipv4Address.fromString(".192.168.1.1")
    assert(result.isEmpty)

  test("Ipv4Address.fromString returns None for double dots"):
    val result = Ipv4Address.fromString("192..168.1")
    assert(result.isEmpty)

  test("Ipv4Address.parse returns Left for invalid input"):
    val result = Ipv4Address.parse("invalid")
    assert(result.isLeft)
    result.left.foreach { err =>
      assert(err.isInstanceOf[AddressError.InvalidIpv4])
    }

  test("Ipv4Address.parse error contains input"):
    val result = Ipv4Address.parse("bad.input")
    result.left.foreach { err =>
      val invalidIpv4 = err.asInstanceOf[AddressError.InvalidIpv4]
      assertEquals(invalidIpv4.input, "bad.input")
    }

  // ============================================================
  // fromOctetsRuntime tests
  // ============================================================

  test("Ipv4Address.fromOctetsRuntime accepts valid octets"):
    val result = Ipv4Address.fromOctetsRuntime(192, 168, 1, 1)
    assert(result.isRight)
    assertEquals(result.map(_.show), Right("192.168.1.1"))

  test("Ipv4Address.fromOctetsRuntime accepts boundary values"):
    val result = Ipv4Address.fromOctetsRuntime(0, 0, 0, 0)
    assert(result.isRight)
    val result2 = Ipv4Address.fromOctetsRuntime(255, 255, 255, 255)
    assert(result2.isRight)

  test("Ipv4Address.fromOctetsRuntime rejects octet > 255"):
    val result = Ipv4Address.fromOctetsRuntime(256, 0, 0, 1)
    assert(result.isLeft)
    val result2 = Ipv4Address.fromOctetsRuntime(0, 0, 0, 300)
    assert(result2.isLeft)

  test("Ipv4Address.fromOctetsRuntime rejects negative octet"):
    val result = Ipv4Address.fromOctetsRuntime(-1, 0, 0, 1)
    assert(result.isLeft)
    val result2 = Ipv4Address.fromOctetsRuntime(0, 0, 0, -100)
    assert(result2.isLeft)

  // ============================================================
  // fromBytes tests
  // ============================================================

  test("Ipv4Address.fromBytes accepts valid 4-byte array"):
    val bytes = Array[Byte](192.toByte, 168.toByte, 1, 1)
    val result = Ipv4Address.fromBytes(bytes)
    assert(result.isDefined)
    assertEquals(result.get.show, "192.168.1.1")

  test("Ipv4Address.fromBytes handles unsigned bytes correctly"):
    val bytes = Array[Byte](255.toByte, 255.toByte, 255.toByte, 255.toByte)
    val result = Ipv4Address.fromBytes(bytes)
    assert(result.isDefined)
    assertEquals(result.get.show, "255.255.255.255")

  test("Ipv4Address.fromBytes returns None for wrong length"):
    assert(Ipv4Address.fromBytes(Array[Byte](1, 2, 3)).isEmpty)
    assert(Ipv4Address.fromBytes(Array[Byte](1, 2, 3, 4, 5)).isEmpty)
    assert(Ipv4Address.fromBytes(Array[Byte]()).isEmpty)

  // ============================================================
  // fromInt tests
  // ============================================================

  test("Ipv4Address.fromInt creates address from integer"):
    val addr = Ipv4Address.fromInt(0x7F000001) // 127.0.0.1
    assertEquals(addr.show, "127.0.0.1")

  test("Ipv4Address.fromInt handles full range"):
    val min = Ipv4Address.fromInt(0)
    assertEquals(min.show, "0.0.0.0")
    val max = Ipv4Address.fromInt(-1) // 0xFFFFFFFF
    assertEquals(max.show, "255.255.255.255")

  // ============================================================
  // octet extraction tests
  // ============================================================

  test("Ipv4Address.octet1-4 extract correct bytes"):
    val addr = Ipv4Address.fromString("192.168.1.100").get
    assertEquals(addr.octet1, 192)
    assertEquals(addr.octet2, 168)
    assertEquals(addr.octet3, 1)
    assertEquals(addr.octet4, 100)

  test("Ipv4Address.octet1-4 handle high values correctly"):
    val addr = Ipv4Address.fromString("255.254.253.252").get
    assertEquals(addr.octet1, 255)
    assertEquals(addr.octet2, 254)
    assertEquals(addr.octet3, 253)
    assertEquals(addr.octet4, 252)

  test("Ipv4Address.octet1-4 handle zero correctly"):
    val addr = Ipv4Address.fromString("0.0.0.0").get
    assertEquals(addr.octet1, 0)
    assertEquals(addr.octet2, 0)
    assertEquals(addr.octet3, 0)
    assertEquals(addr.octet4, 0)

  // ============================================================
  // toBytes / toInt roundtrip tests
  // ============================================================

  test("Ipv4Address.toBytes returns correct bytes"):
    val addr = Ipv4Address.fromString("192.168.1.1").get
    val bytes = addr.toBytes
    assertEquals(bytes.length, 4)
    assertEquals(bytes(0) & 0xff, 192)
    assertEquals(bytes(1) & 0xff, 168)
    assertEquals(bytes(2) & 0xff, 1)
    assertEquals(bytes(3) & 0xff, 1)

  test("Ipv4Address.toBytes roundtrips correctly"):
    val original = Ipv4Address.fromString("10.20.30.40").get
    val roundtripped = Ipv4Address.fromBytes(original.toBytes).get
    assertEquals(roundtripped, original)
    assertEquals(roundtripped.show, original.show)

  test("Ipv4Address.toInt roundtrips correctly"):
    val original = Ipv4Address.fromString("192.168.255.1").get
    val int = original.toInt
    val roundtripped = Ipv4Address.fromInt(int)
    assertEquals(roundtripped, original)

  test("Ipv4Address.toInt returns expected value for loopback"):
    val addr = Ipv4Address.fromString("127.0.0.1").get
    assertEquals(addr.toInt, 0x7F000001)

  // ============================================================
  // show tests
  // ============================================================

  test("Ipv4Address.show produces dotted decimal notation"):
    val addr = Ipv4Address.fromString("10.20.30.40").get
    assertEquals(addr.show, "10.20.30.40")

  test("Ipv4Address.show does not zero-pad"):
    val addr = Ipv4Address.fromString("1.2.3.4").get
    assertEquals(addr.show, "1.2.3.4")

  // ============================================================
  // isLoopback tests (127.0.0.0/8)
  // ============================================================

  test("Ipv4Address.isLoopback for 127.0.0.1"):
    val addr = Ipv4Address.fromString("127.0.0.1").get
    assert(addr.isLoopback)

  test("Ipv4Address.isLoopback for 127.255.255.255"):
    val addr = Ipv4Address.fromString("127.255.255.255").get
    assert(addr.isLoopback)

  test("Ipv4Address.isLoopback for 127.0.0.0"):
    val addr = Ipv4Address.fromString("127.0.0.0").get
    assert(addr.isLoopback)

  test("Ipv4Address.isLoopback is false for 126.255.255.255"):
    val addr = Ipv4Address.fromString("126.255.255.255").get
    assert(!addr.isLoopback)

  test("Ipv4Address.isLoopback is false for 128.0.0.1"):
    val addr = Ipv4Address.fromString("128.0.0.1").get
    assert(!addr.isLoopback)

  // ============================================================
  // isPrivate tests (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
  // ============================================================

  test("Ipv4Address.isPrivate for 10.x.x.x range"):
    assert(Ipv4Address.fromString("10.0.0.0").get.isPrivate)
    assert(Ipv4Address.fromString("10.255.255.255").get.isPrivate)
    assert(Ipv4Address.fromString("10.1.2.3").get.isPrivate)

  test("Ipv4Address.isPrivate for 172.16.0.0/12 range"):
    assert(Ipv4Address.fromString("172.16.0.0").get.isPrivate)
    assert(Ipv4Address.fromString("172.31.255.255").get.isPrivate)
    assert(Ipv4Address.fromString("172.20.1.1").get.isPrivate)

  test("Ipv4Address.isPrivate boundary for 172.16/12"):
    // 172.15.x.x is NOT private
    assert(!Ipv4Address.fromString("172.15.255.255").get.isPrivate)
    // 172.32.x.x is NOT private
    assert(!Ipv4Address.fromString("172.32.0.0").get.isPrivate)

  test("Ipv4Address.isPrivate for 192.168.x.x range"):
    assert(Ipv4Address.fromString("192.168.0.0").get.isPrivate)
    assert(Ipv4Address.fromString("192.168.255.255").get.isPrivate)
    assert(Ipv4Address.fromString("192.168.1.100").get.isPrivate)

  test("Ipv4Address.isPrivate is false for public addresses"):
    assert(!Ipv4Address.fromString("8.8.8.8").get.isPrivate)
    assert(!Ipv4Address.fromString("1.1.1.1").get.isPrivate)
    assert(!Ipv4Address.fromString("192.169.0.0").get.isPrivate)

  // ============================================================
  // isLinkLocal tests (169.254.0.0/16)
  // ============================================================

  test("Ipv4Address.isLinkLocal for 169.254.x.x"):
    assert(Ipv4Address.fromString("169.254.0.0").get.isLinkLocal)
    assert(Ipv4Address.fromString("169.254.255.255").get.isLinkLocal)
    assert(Ipv4Address.fromString("169.254.100.50").get.isLinkLocal)

  test("Ipv4Address.isLinkLocal boundary"):
    assert(!Ipv4Address.fromString("169.253.255.255").get.isLinkLocal)
    assert(!Ipv4Address.fromString("169.255.0.0").get.isLinkLocal)

  // ============================================================
  // isMulticast tests (224.0.0.0/4)
  // ============================================================

  test("Ipv4Address.isMulticast for 224-239 range"):
    assert(Ipv4Address.fromString("224.0.0.0").get.isMulticast)
    assert(Ipv4Address.fromString("239.255.255.255").get.isMulticast)
    assert(Ipv4Address.fromString("230.1.2.3").get.isMulticast)

  test("Ipv4Address.isMulticast boundary"):
    assert(!Ipv4Address.fromString("223.255.255.255").get.isMulticast)
    assert(!Ipv4Address.fromString("240.0.0.0").get.isMulticast)

  // ============================================================
  // isWildcard tests (0.0.0.0)
  // ============================================================

  test("Ipv4Address.isWildcard for 0.0.0.0"):
    assert(Ipv4Address.fromString("0.0.0.0").get.isWildcard)

  test("Ipv4Address.isWildcard is false for non-zero"):
    assert(!Ipv4Address.fromString("0.0.0.1").get.isWildcard)
    assert(!Ipv4Address.fromString("1.0.0.0").get.isWildcard)

  // ============================================================
  // isBroadcast tests (255.255.255.255)
  // ============================================================

  test("Ipv4Address.isBroadcast for 255.255.255.255"):
    assert(Ipv4Address.fromString("255.255.255.255").get.isBroadcast)

  test("Ipv4Address.isBroadcast is false for other addresses"):
    assert(!Ipv4Address.fromString("255.255.255.254").get.isBroadcast)
    assert(!Ipv4Address.fromString("255.255.254.255").get.isBroadcast)

  // ============================================================
  // Constants tests
  // ============================================================

  test("Ipv4Address.Wildcard is 0.0.0.0"):
    assertEquals(Ipv4Address.Wildcard.show, "0.0.0.0")
    assert(Ipv4Address.Wildcard.isWildcard)

  test("Ipv4Address.Loopback is 127.0.0.1"):
    assertEquals(Ipv4Address.Loopback.show, "127.0.0.1")
    assert(Ipv4Address.Loopback.isLoopback)

  test("Ipv4Address.Broadcast is 255.255.255.255"):
    assertEquals(Ipv4Address.Broadcast.show, "255.255.255.255")
    assert(Ipv4Address.Broadcast.isBroadcast)

  // ============================================================
  // Ordering tests (unsigned comparison)
  // ============================================================

  test("Ipv4Address.Ordering compares correctly"):
    val addr1 = Ipv4Address.fromString("10.0.0.1").get
    val addr2 = Ipv4Address.fromString("10.0.0.2").get
    assert(Ordering[Ipv4Address].lt(addr1, addr2))

  test("Ipv4Address.Ordering treats high addresses as greater (unsigned)"):
    // 255.255.255.255 should be greater than 1.0.0.0
    val low = Ipv4Address.fromString("1.0.0.0").get
    val high = Ipv4Address.fromString("255.255.255.255").get
    assert(Ordering[Ipv4Address].lt(low, high))
    assert(Ordering[Ipv4Address].gt(high, low))

  test("Ipv4Address.Ordering handles 128+ addresses correctly"):
    // This tests unsigned comparison: 128.0.0.0 > 127.255.255.255
    val a = Ipv4Address.fromString("127.255.255.255").get
    val b = Ipv4Address.fromString("128.0.0.0").get
    assert(Ordering[Ipv4Address].lt(a, b))

  test("Ipv4Address.Ordering sorts list correctly"):
    val addrs = List("10.0.0.1", "1.0.0.0", "192.168.1.1", "127.0.0.1", "255.0.0.0")
      .flatMap(Ipv4Address.fromString)
    val sorted = addrs.sorted.map(_.show)
    assertEquals(sorted, List("1.0.0.0", "10.0.0.1", "127.0.0.1", "192.168.1.1", "255.0.0.0"))

  // ============================================================
  // Equality tests
  // ============================================================

  test("Ipv4Address equality for same address"):
    val a1 = Ipv4Address.fromString("192.168.1.1").get
    val a2 = Ipv4Address.fromString("192.168.1.1").get
    assertEquals(a1, a2)
    assert(a1 == a2)

  test("Ipv4Address equality via different constructors"):
    val fromString = Ipv4Address.fromString("192.168.1.1").get
    val fromBytes = Ipv4Address.fromBytes(Array[Byte](192.toByte, 168.toByte, 1, 1)).get
    val fromInt = Ipv4Address.fromInt(0xC0A80101)
    assertEquals(fromString, fromBytes)
    assertEquals(fromString, fromInt)

  test("Ipv4Address inequality for different addresses"):
    val a1 = Ipv4Address.fromString("192.168.1.1").get
    val a2 = Ipv4Address.fromString("192.168.1.2").get
    assertNotEquals(a1, a2)

end Ipv4AddressSpec
