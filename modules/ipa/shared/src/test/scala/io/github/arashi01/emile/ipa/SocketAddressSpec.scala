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
 * Tests for SocketAddress enum type.
 *
 * Tests cover:
 * - v4 / v6 constructors
 * - port extraction
 * - isV4 / isV6 checks
 * - fold operation
 * - show: string representation
 * - toIpv4 / toIpv6 extraction
 * - withPort modification
 * - fromString parsing (IPv4:port and [IPv6]:port formats)
 * - Convenience constructors: localhost, localhost6, any, any6
 * - Wildcard constant
 */
class SocketAddressSpec extends FunSuite:

  // ============================================================
  // v4 constructor tests
  // ============================================================

  test("SocketAddress.v4 creates IPv4 socket address"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(8080).toOption.get)
    assert(addr.isV4)
    assert(!addr.isV6)

  test("SocketAddress.v4 preserves address and port"):
    val ip = Ipv4Address.fromString("192.168.1.1").get
    val port = Port.fromInt(443).toOption.get
    val addr = SocketAddress.v4(ip, port)
    assertEquals(addr.port, port)
    assertEquals(addr.toIpv4, Some(ip))

  // ============================================================
  // v6 constructor tests
  // ============================================================

  test("SocketAddress.v6 creates IPv6 socket address (simple)"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(8080).toOption.get)
    assert(addr.isV6)
    assert(!addr.isV4)

  test("SocketAddress.v6 preserves address and port"):
    val ip = Ipv6Address.fromString("2001:db8::1").get
    val port = Port.fromInt(443).toOption.get
    val addr = SocketAddress.v6(ip, port)
    assertEquals(addr.port, port)
    assertEquals(addr.toIpv6, Some(ip))

  test("SocketAddress.v6 with explicit flowInfo and scopeId"):
    val ip = Ipv6Address.fromString("fe80::1").get
    val port = Port.fromInt(80).toOption.get
    val flowInfo = FlowInfo(0x12345)
    val scopeId = ScopeId(2)
    val addr = SocketAddress.v6(ip, port, flowInfo, scopeId)
    addr match
      case SocketAddress.V6(a, p, fi, sid) =>
        assertEquals(a.show, ip.show)
        assertEquals(p, port)
        assertEquals(fi, flowInfo)
        assertEquals(sid, scopeId)
      case _ => fail("Expected V6")

  // ============================================================
  // port extraction tests
  // ============================================================

  test("SocketAddress.port extracts from V4"):
    val port = Port.fromInt(3000).toOption.get
    val addr = SocketAddress.v4(Ipv4Address.Loopback, port)
    assertEquals(addr.port, port)

  test("SocketAddress.port extracts from V6"):
    val port = Port.fromInt(5432).toOption.get
    val addr = SocketAddress.v6(Ipv6Address.Loopback, port)
    assertEquals(addr.port, port)

  // ============================================================
  // isV4 / isV6 tests
  // ============================================================

  test("SocketAddress.isV4 returns true for V4"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.Wildcard)
    assert(addr.isV4)
    assert(!addr.isV6)

  test("SocketAddress.isV6 returns true for V6"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.Wildcard)
    assert(addr.isV6)
    assert(!addr.isV4)

  // ============================================================
  // fold tests
  // ============================================================

  test("SocketAddress.fold invokes fv4 for V4 address"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(80).toOption.get)
    val result = addr.fold((ip, p) => s"v4:${ip.show}:${p.value}") { (_, _, _, _) =>
      "v6"
    }
    assertEquals(result, "v4:127.0.0.1:80")

  test("SocketAddress.fold invokes fv6 for V6 address"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(443).toOption.get)
    val result = addr.fold((_, _) => "v4") { (ip, p, fi, sid) =>
      s"v6:${ip.show}:${p.value}"
    }
    assertEquals(result, "v6:::1:443")

  test("SocketAddress.fold provides all V6 components"):
    val flowInfo = FlowInfo(100)
    val scopeId = ScopeId(2)
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.Wildcard, flowInfo, scopeId)
    val result = addr.fold((_, _) => (0, 0)) { (_, _, fi, sid) =>
      (fi.value, sid.value)
    }
    assertEquals(result, (100, 2))

  // ============================================================
  // show tests
  // ============================================================

  test("SocketAddress.show for V4"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(8080).toOption.get)
    assertEquals(addr.show, "127.0.0.1:8080")

  test("SocketAddress.show for V6"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(443).toOption.get)
    assertEquals(addr.show, "[::1]:443")

  test("SocketAddress.show for V6 with complex address"):
    val ip = Ipv6Address.fromString("2001:db8::1").get
    val addr = SocketAddress.v6(ip, Port.fromInt(80).toOption.get)
    assertEquals(addr.show, "[2001:db8::1]:80")

  test("SocketAddress.show for wildcard addresses"):
    val v4 = SocketAddress.v4(Ipv4Address.Wildcard, Port.Wildcard)
    assertEquals(v4.show, "0.0.0.0:0")
    val v6 = SocketAddress.v6(Ipv6Address.Wildcard, Port.Wildcard)
    assertEquals(v6.show, "[::]:0")

  // ============================================================
  // toIpv4 / toIpv6 tests
  // ============================================================

  test("SocketAddress.toIpv4 returns Some for V4"):
    val ip = Ipv4Address.fromString("10.0.0.1").get
    val addr = SocketAddress.v4(ip, Port.Wildcard)
    assertEquals(addr.toIpv4, Some(ip))

  test("SocketAddress.toIpv4 returns None for V6"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.Wildcard)
    assertEquals(addr.toIpv4, None)

  test("SocketAddress.toIpv6 returns Some for V6"):
    val ip = Ipv6Address.fromString("fe80::1").get
    val addr = SocketAddress.v6(ip, Port.Wildcard)
    assertEquals(addr.toIpv6, Some(ip))

  test("SocketAddress.toIpv6 returns None for V4"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.Wildcard)
    assertEquals(addr.toIpv6, None)

  // ============================================================
  // withPort tests
  // ============================================================

  test("SocketAddress.withPort creates new V4 with different port"):
    val original = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(80).toOption.get)
    val newPort = Port.fromInt(443).toOption.get
    val modified = original.withPort(newPort)
    assertEquals(modified.port, newPort)
    assertEquals(modified.toIpv4, original.toIpv4)
    assert(modified.isV4)

  test("SocketAddress.withPort creates new V6 with different port"):
    val flowInfo = FlowInfo(100)
    val scopeId = ScopeId(2)
    val original = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(80).toOption.get, flowInfo, scopeId)
    val newPort = Port.fromInt(8080).toOption.get
    val modified = original.withPort(newPort)
    assertEquals(modified.port, newPort)
    // FlowInfo and ScopeId should be preserved
    modified match
      case SocketAddress.V6(_, _, fi, sid) =>
        assertEquals(fi, flowInfo)
        assertEquals(sid, scopeId)
      case _ => fail("Expected V6")

  // ============================================================
  // fromString tests - IPv4
  // ============================================================

  test("SocketAddress.fromString parses IPv4:port"):
    val result = SocketAddress.fromString("192.168.1.1:8080")
    assert(result.isRight)
    result.foreach { addr =>
      assert(addr.isV4)
      assertEquals(addr.show, "192.168.1.1:8080")
    }

  test("SocketAddress.fromString parses localhost:port"):
    val result = SocketAddress.fromString("127.0.0.1:80")
    assert(result.isRight)
    result.foreach { addr =>
      assertEquals(addr.port.value, 80)
    }

  test("SocketAddress.fromString returns Left for missing port separator"):
    val result = SocketAddress.fromString("192.168.1.1")
    assert(result.isLeft)

  test("SocketAddress.fromString returns Left for invalid IPv4"):
    val result = SocketAddress.fromString("256.1.1.1:80")
    assert(result.isLeft)

  test("SocketAddress.fromString returns Left for invalid port"):
    val result = SocketAddress.fromString("192.168.1.1:99999")
    assert(result.isLeft)

  test("SocketAddress.fromString returns Left for empty string"):
    val result = SocketAddress.fromString("")
    assert(result.isLeft)

  test("SocketAddress.fromString returns Left for null"):
    val result = SocketAddress.fromString(null)
    assert(result.isLeft)

  // ============================================================
  // fromString tests - IPv6
  // ============================================================

  test("SocketAddress.fromString parses [IPv6]:port"):
    val result = SocketAddress.fromString("[::1]:8080")
    assert(result.isRight)
    result.foreach { addr =>
      assert(addr.isV6)
      assertEquals(addr.show, "[::1]:8080")
    }

  test("SocketAddress.fromString parses [2001:db8::1]:port"):
    val result = SocketAddress.fromString("[2001:db8::1]:443")
    assert(result.isRight)
    result.foreach { addr =>
      assertEquals(addr.port.value, 443)
    }

  test("SocketAddress.fromString parses [::]:port"):
    val result = SocketAddress.fromString("[::]:0")
    assert(result.isRight)
    result.foreach { addr =>
      assertEquals(addr.show, "[::]:0")
    }

  test("SocketAddress.fromString returns Left for missing closing bracket"):
    val result = SocketAddress.fromString("[::1:8080")
    assert(result.isLeft)

  test("SocketAddress.fromString returns Left for missing port after bracket"):
    val result = SocketAddress.fromString("[::1]")
    assert(result.isLeft)

  test("SocketAddress.fromString returns Left for invalid IPv6"):
    val result = SocketAddress.fromString("[invalid]:80")
    assert(result.isLeft)

  // ============================================================
  // Convenience constructors tests
  // ============================================================

  test("SocketAddress.localhost creates 127.0.0.1:port"):
    val port = Port.fromInt(3000).toOption.get
    val addr = SocketAddress.localhost(port)
    assertEquals(addr.show, "127.0.0.1:3000")
    assert(addr.isV4)

  test("SocketAddress.localhost6 creates [::1]:port"):
    val port = Port.fromInt(3000).toOption.get
    val addr = SocketAddress.localhost6(port)
    assertEquals(addr.show, "[::1]:3000")
    assert(addr.isV6)

  test("SocketAddress.any creates 0.0.0.0:port"):
    val port = Port.fromInt(8080).toOption.get
    val addr = SocketAddress.any(port)
    assertEquals(addr.show, "0.0.0.0:8080")
    assert(addr.isV4)

  test("SocketAddress.any6 creates [::]:port"):
    val port = Port.fromInt(8080).toOption.get
    val addr = SocketAddress.any6(port)
    assertEquals(addr.show, "[::]:8080")
    assert(addr.isV6)

  // ============================================================
  // Wildcard constant tests
  // ============================================================

  test("SocketAddress.Wildcard is 0.0.0.0:0"):
    assertEquals(SocketAddress.Wildcard.show, "0.0.0.0:0")
    assert(SocketAddress.Wildcard.isV4)
    assertEquals(SocketAddress.Wildcard.port, Port.Wildcard)

  // ============================================================
  // Equality tests
  // ============================================================

  test("SocketAddress equality for V4"):
    val addr1 = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(80).toOption.get)
    val addr2 = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(80).toOption.get)
    assertEquals(addr1, addr2)

  test("SocketAddress inequality for different ports"):
    val addr1 = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(80).toOption.get)
    val addr2 = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(443).toOption.get)
    assertNotEquals(addr1, addr2)

  test("SocketAddress inequality for V4 vs V6"):
    val v4 = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(80).toOption.get)
    val v6 = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(80).toOption.get)
    assertNotEquals(v4, v6)

end SocketAddressSpec
