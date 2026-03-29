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

/** Tests for Ipv4Address opaque type.
  *
  * Tests cover:
  *   - fromString / parse: string parsing with various formats
  *   - fromOctetsRuntime: runtime octet validation
  *   - fromBytes: byte array parsing
  *   - fromInt: raw integer construction
  *   - octet1-4: byte extraction
  *   - toBytes: byte array extraction
  *   - toInt: integer extraction
  *   - show: string representation
  *   - Boolean properties: isLoopback, isPrivate, isLinkLocal, isMulticast, isWildcard, isBroadcast
  *   - Constants: Wildcard, Loopback, Broadcast
  *   - Ordering: unsigned comparison
  */
class Ipv4AddressSpec extends FunSuite:
// scalafix:off

  private def expectRight[A](either: Either[AddressError, A]): A =
    either.fold(err => fail(err.message), identity)

  // ============================================================
  // from(String) tests
  // ============================================================

  test("Ipv4Address.fromString parses valid address"):
    val result = Ipv4Address.from("192.168.1.1")
    assertEquals(expectRight(result).show, "192.168.1.1")

  test("Ipv4Address.fromString parses loopback"):
    val result = Ipv4Address.from("127.0.0.1")
    assertEquals(expectRight(result).show, "127.0.0.1")

  test("Ipv4Address.fromString parses wildcard"):
    val result = Ipv4Address.from("0.0.0.0")
    assertEquals(expectRight(result).show, "0.0.0.0")

  test("Ipv4Address.fromString parses broadcast"):
    val result = Ipv4Address.from("255.255.255.255")
    assertEquals(expectRight(result).show, "255.255.255.255")

  test("Ipv4Address.fromString returns None for empty string"):
    val result = Ipv4Address.from("")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for whitespace"):
    val result = Ipv4Address.from("   ")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for null"):
    val result = Ipv4Address.from(null: String)
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for too few octets"):
    val result = Ipv4Address.from("192.168.1")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for too many octets"):
    val result = Ipv4Address.from("192.168.1.1.1")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for octet > 255"):
    val result = Ipv4Address.from("192.168.1.256")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for negative octet"):
    val result = Ipv4Address.from("192.168.1.-1")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for non-numeric octet"):
    val result = Ipv4Address.from("192.168.1.abc")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for trailing dot"):
    val result = Ipv4Address.from("192.168.1.1.")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for leading dot"):
    val result = Ipv4Address.from(".192.168.1.1")
    assert(result.isLeft)

  test("Ipv4Address.fromString returns None for double dots"):
    val result = Ipv4Address.from("192..168.1")
    assert(result.isLeft)

  test("Ipv4Address.parse returns Left for invalid input"):
    val result = Ipv4Address.from("invalid")
    assert(result.isLeft)
    result.left.foreach { err =>
      assert(err.isInstanceOf[AddressError.InvalidIpv4])
    }

  test("Ipv4Address.parse error contains input"):
    val result = Ipv4Address.from("bad.input")
    result.left.foreach { err =>
      val invalidIpv4 = err.asInstanceOf[AddressError.InvalidIpv4]
      assertEquals(invalidIpv4.input, "bad.input")
    }

  // ============================================================
  // from(Int, Int, Int, Int) tests
  // ============================================================

  test("Ipv4Address.from(Int,Int,Int,Int) accepts valid octets"):
    val result = Ipv4Address.from(192, 168, 1, 1)
    assert(result.isRight)
    assertEquals(result.map(_.show), Right("192.168.1.1"))

  test("Ipv4Address.from(Int,Int,Int,Int) accepts boundary values"):
    val result = Ipv4Address.from(0, 0, 0, 0)
    assert(result.isRight)
    val result2 = Ipv4Address.from(255, 255, 255, 255)
    assert(result2.isRight)

  test("Ipv4Address.from(Int,Int,Int,Int) rejects octet > 255"):
    val result = Ipv4Address.from(256, 0, 0, 1)
    assert(result.isLeft)
    val result2 = Ipv4Address.from(0, 0, 0, 300)
    assert(result2.isLeft)

  test("Ipv4Address.from(Int,Int,Int,Int) rejects negative octet"):
    val result = Ipv4Address.from(-1, 0, 0, 1)
    assert(result.isLeft)
    val result2 = Ipv4Address.from(0, 0, 0, -100)
    assert(result2.isLeft)

  // ============================================================
  // fromBytes tests
  // ============================================================

  test("Ipv4Address.fromBytes accepts valid 4-byte array"):
    val bytes = Array[Byte](192.toByte, 168.toByte, 1, 1)
    val result = Ipv4Address.from(bytes)
    assertEquals(expectRight(result).show, "192.168.1.1")

  test("Ipv4Address.fromBytes handles unsigned bytes correctly"):
    val bytes = Array[Byte](255.toByte, 255.toByte, 255.toByte, 255.toByte)
    val result = Ipv4Address.from(bytes)
    assertEquals(expectRight(result).show, "255.255.255.255")

  test("Ipv4Address.fromBytes is insensitive to later mutation"):
    val bytes = Array[Byte](10, 20, 30, 40)
    val addr = expectRight(Ipv4Address.from(bytes))
    bytes(0) = 99
    bytes(1) = 99
    assertEquals(addr.show, "10.20.30.40")

  test("Ipv4Address.fromBytes returns error for wrong length"):
    assert(Ipv4Address.from(Array[Byte](1, 2, 3)).isLeft)
    assert(Ipv4Address.from(Array[Byte](1, 2, 3, 4, 5)).isLeft)
    assert(Ipv4Address.from(Array[Byte]()).isLeft)

  // ============================================================
  // fromInt tests
  // ============================================================

  test("Ipv4Address.fromInt creates address from integer"):
    val addr = Ipv4Address.fromInt(0x7f000001) // 127.0.0.1
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
    val addr = expectRight(Ipv4Address.from("192.168.1.100"))
    assertEquals(addr.octet1, 192)
    assertEquals(addr.octet2, 168)
    assertEquals(addr.octet3, 1)
    assertEquals(addr.octet4, 100)

  test("Ipv4Address.octet1-4 handle high values correctly"):
    val addr = expectRight(Ipv4Address.from("255.254.253.252"))
    assertEquals(addr.octet1, 255)
    assertEquals(addr.octet2, 254)
    assertEquals(addr.octet3, 253)
    assertEquals(addr.octet4, 252)

  test("Ipv4Address.octet1-4 handle zero correctly"):
    val addr = expectRight(Ipv4Address.from("0.0.0.0"))
    assertEquals(addr.octet1, 0)
    assertEquals(addr.octet2, 0)
    assertEquals(addr.octet3, 0)
    assertEquals(addr.octet4, 0)

  // ============================================================
  // toBytes / toInt roundtrip tests
  // ============================================================

  test("Ipv4Address.toBytes returns correct bytes"):
    val addr = expectRight(Ipv4Address.from("192.168.1.1"))
    val bytes = addr.toBytes
    assertEquals(bytes.length, 4)
    assertEquals(bytes(0) & 0xff, 192)
    assertEquals(bytes(1) & 0xff, 168)
    assertEquals(bytes(2) & 0xff, 1)
    assertEquals(bytes(3) & 0xff, 1)

  test("Ipv4Address.toBytes roundtrips correctly"):
    val original = expectRight(Ipv4Address.from("10.20.30.40"))
    val roundtripped = expectRight(Ipv4Address.from(original.toBytes))
    assertEquals(roundtripped, original)
    assertEquals(roundtripped.show, original.show)

  test("Ipv4Address.toInt roundtrips correctly"):
    val original = expectRight(Ipv4Address.from("192.168.255.1"))
    val int = original.toInt
    val roundtripped = Ipv4Address.fromInt(int)
    assertEquals(roundtripped, original)

  test("Ipv4Address.toInt returns expected value for loopback"):
    val addr = expectRight(Ipv4Address.from("127.0.0.1"))
    assertEquals(addr.toInt, 0x7f000001)

  // ============================================================
  // show tests
  // ============================================================

  test("Ipv4Address.show produces dotted decimal notation"):
    val addr = expectRight(Ipv4Address.from("10.20.30.40"))
    assertEquals(addr.show, "10.20.30.40")

  test("Ipv4Address.show does not zero-pad"):
    val addr = expectRight(Ipv4Address.from("1.2.3.4"))
    assertEquals(addr.show, "1.2.3.4")

  // ============================================================
  // writeTo tests
  // ============================================================

  test("Ipv4Address.writeTo appends dotted decimal"):
    val addr = expectRight(Ipv4Address.from("10.20.30.40"))
    val sb = new java.lang.StringBuilder("addr=")
    val result = addr.writeTo(sb)
    assertEquals(sb.toString, "addr=10.20.30.40")
    assertEquals(result, sb)

  // ============================================================
  // literals interpolation tests
  // ============================================================

  test("ipv4 interpolator accepts mixed literal and values"):
    import emile.ipa.literals.*

    val octet = 42
    val addr = ipv4"10.0.${octet}.1"
    assertEquals(addr.show, "10.0.42.1")

  test("ipv4 interpolator rejects invalid literal fragments"):
    val errors = compileErrors(
      """import emile.ipa.literals.*

val bad = ipv4"10.a${1}.1"
"""
    )
    assert(errors.contains("Invalid IPv4 literal fragment"))

  // ============================================================
  // isLoopback tests (127.0.0.0/8)
  // ============================================================

  test("Ipv4Address.isLoopback for 127.0.0.1"):
    val addr = expectRight(Ipv4Address.from("127.0.0.1"))
    assert(addr.isLoopback)

  test("Ipv4Address.isLoopback for 127.255.255.255"):
    val addr = expectRight(Ipv4Address.from("127.255.255.255"))
    assert(addr.isLoopback)

  test("Ipv4Address.isLoopback for 127.0.0.0"):
    val addr = expectRight(Ipv4Address.from("127.0.0.0"))
    assert(addr.isLoopback)

  test("Ipv4Address.isLoopback is false for 126.255.255.255"):
    val addr = expectRight(Ipv4Address.from("126.255.255.255"))
    assert(!addr.isLoopback)

  test("Ipv4Address.isLoopback is false for 128.0.0.1"):
    val addr = expectRight(Ipv4Address.from("128.0.0.1"))
    assert(!addr.isLoopback)

  // ============================================================
  // isPrivate tests (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
  // ============================================================

  test("Ipv4Address.isPrivate for 10.x.x.x range"):
    assert(expectRight(Ipv4Address.from("10.0.0.0")).isPrivate)
    assert(expectRight(Ipv4Address.from("10.255.255.255")).isPrivate)
    assert(expectRight(Ipv4Address.from("10.1.2.3")).isPrivate)

  test("Ipv4Address.isPrivate for 172.16.0.0/12 range"):
    assert(expectRight(Ipv4Address.from("172.16.0.0")).isPrivate)
    assert(expectRight(Ipv4Address.from("172.31.255.255")).isPrivate)
    assert(expectRight(Ipv4Address.from("172.20.1.1")).isPrivate)

  test("Ipv4Address.isPrivate boundary for 172.16/12"):
    // 172.15.x.x is NOT private
    assert(!expectRight(Ipv4Address.from("172.15.255.255")).isPrivate)
    // 172.32.x.x is NOT private
    assert(!expectRight(Ipv4Address.from("172.32.0.0")).isPrivate)

  test("Ipv4Address.isPrivate for 192.168.x.x range"):
    assert(expectRight(Ipv4Address.from("192.168.0.0")).isPrivate)
    assert(expectRight(Ipv4Address.from("192.168.255.255")).isPrivate)
    assert(expectRight(Ipv4Address.from("192.168.1.100")).isPrivate)

  test("Ipv4Address.isPrivate is false for public addresses"):
    assert(!expectRight(Ipv4Address.from("8.8.8.8")).isPrivate)
    assert(!expectRight(Ipv4Address.from("1.1.1.1")).isPrivate)
    assert(!expectRight(Ipv4Address.from("192.169.0.0")).isPrivate)

  // ============================================================
  // isLinkLocal tests (169.254.0.0/16)
  // ============================================================

  test("Ipv4Address.isLinkLocal for 169.254.x.x"):
    assert(expectRight(Ipv4Address.from("169.254.0.0")).isLinkLocal)
    assert(expectRight(Ipv4Address.from("169.254.255.255")).isLinkLocal)
    assert(expectRight(Ipv4Address.from("169.254.100.50")).isLinkLocal)

  test("Ipv4Address.isLinkLocal boundary"):
    assert(!expectRight(Ipv4Address.from("169.253.255.255")).isLinkLocal)
    assert(!expectRight(Ipv4Address.from("169.255.0.0")).isLinkLocal)

  // ============================================================
  // isMulticast tests (224.0.0.0/4)
  // ============================================================

  test("Ipv4Address.isMulticast for 224-239 range"):
    assert(expectRight(Ipv4Address.from("224.0.0.0")).isMulticast)
    assert(expectRight(Ipv4Address.from("239.255.255.255")).isMulticast)
    assert(expectRight(Ipv4Address.from("230.1.2.3")).isMulticast)

  test("Ipv4Address.isMulticast boundary"):
    assert(!expectRight(Ipv4Address.from("223.255.255.255")).isMulticast)
    assert(!expectRight(Ipv4Address.from("240.0.0.0")).isMulticast)

  // ============================================================
  // isWildcard tests (0.0.0.0)
  // ============================================================

  test("Ipv4Address.isWildcard for 0.0.0.0"):
    assert(expectRight(Ipv4Address.from("0.0.0.0")).isWildcard)

  test("Ipv4Address.isWildcard is false for non-zero"):
    assert(!expectRight(Ipv4Address.from("0.0.0.1")).isWildcard)
    assert(!expectRight(Ipv4Address.from("1.0.0.0")).isWildcard)

  // ============================================================
  // isBroadcast tests (255.255.255.255)
  // ============================================================

  test("Ipv4Address.isBroadcast for 255.255.255.255"):
    assert(expectRight(Ipv4Address.from("255.255.255.255")).isBroadcast)

  test("Ipv4Address.isBroadcast is false for other addresses"):
    assert(!expectRight(Ipv4Address.from("255.255.255.254")).isBroadcast)
    assert(!expectRight(Ipv4Address.from("255.255.254.255")).isBroadcast)

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
    val addr1 = expectRight(Ipv4Address.from("10.0.0.1"))
    val addr2 = expectRight(Ipv4Address.from("10.0.0.2"))
    assert(Ordering[Ipv4Address].lt(addr1, addr2))

  test("Ipv4Address.Ordering treats high addresses as greater (unsigned)"):
    // 255.255.255.255 should be greater than 1.0.0.0
    val low = expectRight(Ipv4Address.from("1.0.0.0"))
    val high = expectRight(Ipv4Address.from("255.255.255.255"))
    assert(Ordering[Ipv4Address].lt(low, high))
    assert(Ordering[Ipv4Address].gt(high, low))

  test("Ipv4Address.Ordering handles 128+ addresses correctly"):
    // This tests unsigned comparison: 128.0.0.0 > 127.255.255.255
    val a = expectRight(Ipv4Address.from("127.255.255.255"))
    val b = expectRight(Ipv4Address.from("128.0.0.0"))
    assert(Ordering[Ipv4Address].lt(a, b))

  test("Ipv4Address.Ordering sorts list correctly"):
    val addrs = List("10.0.0.1", "1.0.0.0", "192.168.1.1", "127.0.0.1", "255.0.0.0")
      .map(s => expectRight(Ipv4Address.from(s)))
    val sorted = addrs.sorted.map(_.show)
    assertEquals(sorted, List("1.0.0.0", "10.0.0.1", "127.0.0.1", "192.168.1.1", "255.0.0.0"))

  // ============================================================
  // Equality tests
  // ============================================================

  test("Ipv4Address equality for same address"):
    val a1 = expectRight(Ipv4Address.from("192.168.1.1"))
    val a2 = expectRight(Ipv4Address.from("192.168.1.1"))
    assertEquals(a1, a2)
    assert(a1 == a2)

  test("Ipv4Address equality via different constructors"):
    val fromString = expectRight(Ipv4Address.from("192.168.1.1"))
    val fromBytes = expectRight(Ipv4Address.from(Array[Byte](192.toByte, 168.toByte, 1, 1)))
    val fromInt = Ipv4Address.fromInt(0xc0a80101)
    assertEquals(fromString, fromBytes)
    assertEquals(fromString, fromInt)

  test("Ipv4Address inequality for different addresses"):
    val a1 = expectRight(Ipv4Address.from("192.168.1.1"))
    val a2 = expectRight(Ipv4Address.from("192.168.1.2"))
    assertNotEquals(a1, a2)

end Ipv4AddressSpec
