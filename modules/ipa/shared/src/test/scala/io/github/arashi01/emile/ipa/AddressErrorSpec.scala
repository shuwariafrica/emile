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
 * Tests for AddressError enum type.
 *
 * Tests cover:
 * - message formatting for each variant
 * - Equality
 */
class AddressErrorSpec extends FunSuite:

  // ============================================================
  // InvalidPort tests
  // ============================================================

  test("AddressError.InvalidPort message includes value"):
    val err = AddressError.InvalidPort(-1)
    val msg = err.message
    assert(msg.contains("-1"))
    assert(msg.contains("Invalid port"))
    assert(msg.contains("0-65535"))

  test("AddressError.InvalidPort preserves value"):
    val err = AddressError.InvalidPort(70000)
    err match
      case AddressError.InvalidPort(v) => assertEquals(v, 70000)
      case _ => fail("Expected InvalidPort")

  test("AddressError.InvalidPort equality"):
    val err1 = AddressError.InvalidPort(100)
    val err2 = AddressError.InvalidPort(100)
    assertEquals(err1, err2)

  test("AddressError.InvalidPort inequality"):
    val err1 = AddressError.InvalidPort(100)
    val err2 = AddressError.InvalidPort(200)
    assertNotEquals(err1, err2)

  // ============================================================
  // InvalidIpv4 tests
  // ============================================================

  test("AddressError.InvalidIpv4 message includes input and detail"):
    val err = AddressError.InvalidIpv4("bad.input", "too few octets")
    val msg = err.message
    assert(msg.contains("bad.input"))
    assert(msg.contains("too few octets"))
    assert(msg.contains("Invalid IPv4"))

  test("AddressError.InvalidIpv4 preserves input"):
    val err = AddressError.InvalidIpv4("256.1.1.1", "octet out of range")
    err match
      case AddressError.InvalidIpv4(i, _) => assertEquals(i, "256.1.1.1")
      case _ => fail("Expected InvalidIpv4")

  test("AddressError.InvalidIpv4 preserves detail"):
    val err = AddressError.InvalidIpv4("a.b.c.d", "non-numeric octet")
    err match
      case AddressError.InvalidIpv4(_, d) => assertEquals(d, "non-numeric octet")
      case _ => fail("Expected InvalidIpv4")

  test("AddressError.InvalidIpv4 equality"):
    val err1 = AddressError.InvalidIpv4("x", "y")
    val err2 = AddressError.InvalidIpv4("x", "y")
    assertEquals(err1, err2)

  // ============================================================
  // InvalidIpv6 tests
  // ============================================================

  test("AddressError.InvalidIpv6 message includes input and detail"):
    val err = AddressError.InvalidIpv6(":::1", "triple colon")
    val msg = err.message
    assert(msg.contains(":::1"))
    assert(msg.contains("triple colon"))
    assert(msg.contains("Invalid IPv6"))

  test("AddressError.InvalidIpv6 preserves input"):
    val err = AddressError.InvalidIpv6("ghij::1", "invalid hex")
    err match
      case AddressError.InvalidIpv6(i, _) => assertEquals(i, "ghij::1")
      case _ => fail("Expected InvalidIpv6")

  test("AddressError.InvalidIpv6 preserves detail"):
    val err = AddressError.InvalidIpv6("1:2:3:4:5:6:7:8:9", "too many groups")
    err match
      case AddressError.InvalidIpv6(_, d) => assertEquals(d, "too many groups")
      case _ => fail("Expected InvalidIpv6")

  test("AddressError.InvalidIpv6 equality"):
    val err1 = AddressError.InvalidIpv6("a", "b")
    val err2 = AddressError.InvalidIpv6("a", "b")
    assertEquals(err1, err2)

  // ============================================================
  // InvalidSocketAddress tests
  // ============================================================

  test("AddressError.InvalidSocketAddress message includes input and detail"):
    val err = AddressError.InvalidSocketAddress("192.168.1.1", "missing port")
    val msg = err.message
    assert(msg.contains("192.168.1.1"))
    assert(msg.contains("missing port"))
    assert(msg.contains("Invalid socket address"))

  test("AddressError.InvalidSocketAddress preserves input"):
    val err = AddressError.InvalidSocketAddress("[::1", "missing bracket")
    err match
      case AddressError.InvalidSocketAddress(i, _) => assertEquals(i, "[::1")
      case _ => fail("Expected InvalidSocketAddress")

  test("AddressError.InvalidSocketAddress preserves detail"):
    val err = AddressError.InvalidSocketAddress("bad", "invalid format")
    err match
      case AddressError.InvalidSocketAddress(_, d) => assertEquals(d, "invalid format")
      case _ => fail("Expected InvalidSocketAddress")

  test("AddressError.InvalidSocketAddress equality"):
    val err1 = AddressError.InvalidSocketAddress("x", "y")
    val err2 = AddressError.InvalidSocketAddress("x", "y")
    assertEquals(err1, err2)

  // ============================================================
  // Cross-variant inequality tests
  // ============================================================

  test("Different error variants are not equal"):
    val portErr = AddressError.InvalidPort(100)
    val ipv4Err = AddressError.InvalidIpv4("x", "y")
    val ipv6Err = AddressError.InvalidIpv6("x", "y")
    val sockErr = AddressError.InvalidSocketAddress("x", "y")

    assertNotEquals(portErr, ipv4Err)
    assertNotEquals(ipv4Err, ipv6Err)
    assertNotEquals(ipv6Err, sockErr)

end AddressErrorSpec
