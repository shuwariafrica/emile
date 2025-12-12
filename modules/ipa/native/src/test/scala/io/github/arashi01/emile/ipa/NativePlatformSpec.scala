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

import scala.scalanative.posix.arpa.inet.*
import scala.scalanative.posix.netinet.in.*
import scala.scalanative.posix.netinet.inOps.*
import scala.scalanative.posix.sys.socket.*
import scala.scalanative.posix.sys.socketOps.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/**
 * Platform-specific tests for Native sockaddr conversions.
 *
 * These tests exercise actual native memory operations and verify correct
 * byte-order conversion (host <-> network order) for IPv4 and IPv6 addresses.
 *
 * Tests cover:
 * - Ipv4Address.toNetworkOrder: byte order conversion
 * - Ipv4Address.fillSockAddrIn: sockaddr_in population
 * - Ipv6Address.fillNetworkOrder: 16-byte buffer fill
 * - Ipv6Address.fillSockAddrIn6: sockaddr_in6 population
 * - SocketAddress.toSockAddr: allocation and population
 * - SocketAddress.sockAddrSize: size calculation
 * - native.fromSockAddr: parsing back to SocketAddress
 * - Full roundtrip tests
 */
class NativePlatformSpec extends FunSuite:

  // ============================================================
  // Ipv4Address.toNetworkOrder tests
  // ============================================================

  test("Ipv4Address.toNetworkOrder converts to network byte order"):
    val addr = Ipv4Address.fromString("192.168.1.1").get
    val networkOrder = addr.toNetworkOrder
    // Network order is big-endian
    // 192.168.1.1 = 0xC0A80101 in host order
    // In network order (big-endian), bytes are: C0, A8, 01, 01
    val expected = htonl(0xC0A80101.toUInt)
    assertEquals(networkOrder, expected)

  test("Ipv4Address.toNetworkOrder for loopback"):
    val addr = Ipv4Address.Loopback // 127.0.0.1
    val networkOrder = addr.toNetworkOrder
    val expected = htonl(0x7F000001.toUInt)
    assertEquals(networkOrder, expected)

  test("Ipv4Address.toNetworkOrder for wildcard"):
    val addr = Ipv4Address.Wildcard // 0.0.0.0
    val networkOrder = addr.toNetworkOrder
    val expected = htonl(0.toUInt)
    assertEquals(networkOrder, expected)

  test("Ipv4Address.toNetworkOrder for broadcast"):
    val addr = Ipv4Address.Broadcast // 255.255.255.255
    val networkOrder = addr.toNetworkOrder
    val expected = htonl(0xFFFFFFFF.toUInt)
    assertEquals(networkOrder, expected)

  // ============================================================
  // Ipv4Address.fillSockAddrIn tests
  // ============================================================

  test("Ipv4Address.fillSockAddrIn populates sockaddr_in correctly"):
    Zone.acquire { implicit z =>
      val sockaddr = alloc[sockaddr_in]()
      val addr = Ipv4Address.fromString("192.168.1.100").get
      val port = Port.fromInt(8080).toOption.get

      addr.fillSockAddrIn(sockaddr, port)

      assertEquals(sockaddr.sin_family.toInt, AF_INET)
      assertEquals(ntohs(sockaddr.sin_port).toInt, 8080)
      assertEquals(ntohl(sockaddr.sin_addr.s_addr).toInt, 0xC0A80164) // 192.168.1.100
    }

  test("Ipv4Address.fillSockAddrIn for loopback"):
    Zone.acquire { implicit z =>
      val sockaddr = alloc[sockaddr_in]()
      Ipv4Address.Loopback.fillSockAddrIn(sockaddr, Port.fromInt(80).toOption.get)

      assertEquals(sockaddr.sin_family.toInt, AF_INET)
      assertEquals(ntohs(sockaddr.sin_port).toInt, 80)
      assertEquals(ntohl(sockaddr.sin_addr.s_addr).toInt, 0x7F000001)
    }

  // ============================================================
  // Ipv6Address.fillNetworkOrder tests
  // ============================================================

  test("Ipv6Address.fillNetworkOrder fills 16-byte buffer correctly"):
    Zone.acquire { implicit z =>
      val buf = alloc[Byte](16)
      val addr = Ipv6Address.Loopback // ::1

      addr.fillNetworkOrder(buf)

      // ::1 has 15 zero bytes followed by 0x01
      (0 until 15).foreach(i => assertEquals(buf(i).toInt, 0))
      assertEquals(buf(15).toInt, 1)
    }

  test("Ipv6Address.fillNetworkOrder for wildcard"):
    Zone.acquire { implicit z =>
      val buf = alloc[Byte](16)
      Ipv6Address.Wildcard.fillNetworkOrder(buf)

      // All zeros
      (0 until 16).foreach(i => assertEquals(buf(i).toInt, 0))
    }

  test("Ipv6Address.fillNetworkOrder for all-ones"):
    Zone.acquire { implicit z =>
      val buf = alloc[Byte](16)
      val addr = Ipv6Address.fromLongs(-1L, -1L)

      addr.fillNetworkOrder(buf)

      // All 0xFF
      (0 until 16).foreach(i => assertEquals((buf(i) & 0xff), 255))
    }

  test("Ipv6Address.fillNetworkOrder preserves high/low bits"):
    Zone.acquire { implicit z =>
      val buf = alloc[Byte](16)
      val high = 0x20010db800000000L
      val low = 0x0000000000000001L
      val addr = Ipv6Address.fromLongs(high, low)

      addr.fillNetworkOrder(buf)

      // Verify the high bits (first 8 bytes)
      // 2001:0db8:0000:0000 = 0x20, 0x01, 0x0d, 0xb8, 0x00, 0x00, 0x00, 0x00
      assertEquals((buf(0) & 0xff), 0x20)
      assertEquals((buf(1) & 0xff), 0x01)
      assertEquals((buf(2) & 0xff), 0x0d)
      assertEquals((buf(3) & 0xff), 0xb8)
      (4 until 15).foreach(i => assertEquals(buf(i).toInt, 0))
      assertEquals(buf(15).toInt, 1)
    }

  // ============================================================
  // Ipv6Address.fillSockAddrIn6 tests
  // ============================================================

  test("Ipv6Address.fillSockAddrIn6 populates sockaddr_in6 correctly"):
    Zone.acquire { implicit z =>
      val sockaddr = alloc[sockaddr_in6]()
      val addr = Ipv6Address.Loopback
      val port = Port.fromInt(443).toOption.get
      val flowInfo = FlowInfo(0x12345)
      val scopeId = ScopeId(2)

      addr.fillSockAddrIn6(sockaddr, port, flowInfo, scopeId)

      assertEquals(sockaddr.sin6_family.toInt, AF_INET6)
      assertEquals(ntohs(sockaddr.sin6_port).toInt, 443)
      assertEquals(ntohl(sockaddr.sin6_flowinfo).toInt, 0x12345)
      assertEquals(ntohl(sockaddr.sin6_scope_id).toInt, 2)
    }

  test("Ipv6Address.fillSockAddrIn6 with default flowInfo and scopeId"):
    Zone.acquire { implicit z =>
      val sockaddr = alloc[sockaddr_in6]()
      Ipv6Address.Loopback.fillSockAddrIn6(sockaddr, Port.Wildcard, FlowInfo.Default, ScopeId.Default)

      assertEquals(sockaddr.sin6_family.toInt, AF_INET6)
      assertEquals(ntohs(sockaddr.sin6_port).toInt, 0)
      assertEquals(ntohl(sockaddr.sin6_flowinfo).toInt, 0)
      assertEquals(ntohl(sockaddr.sin6_scope_id).toInt, 0)
    }

  // ============================================================
  // SocketAddress.toSockAddr tests
  // ============================================================

  test("SocketAddress.toSockAddr allocates and fills sockaddr_in for V4"):
    Zone.acquire { implicit z =>
      val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(8080).toOption.get)
      val sockaddr = addr.toSockAddr

      assertEquals(sockaddr.sa_family.toInt, AF_INET)
      val sin = sockaddr.asInstanceOf[Ptr[sockaddr_in]]
      assertEquals(ntohs(sin.sin_port).toInt, 8080)
      assertEquals(ntohl(sin.sin_addr.s_addr).toInt, 0x7F000001)
    }

  test("SocketAddress.toSockAddr allocates and fills sockaddr_in6 for V6"):
    Zone.acquire { implicit z =>
      val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(443).toOption.get)
      val sockaddr = addr.toSockAddr

      assertEquals(sockaddr.sa_family.toInt, AF_INET6)
      val sin6 = sockaddr.asInstanceOf[Ptr[sockaddr_in6]]
      assertEquals(ntohs(sin6.sin6_port).toInt, 443)
    }

  // ============================================================
  // SocketAddress.sockAddrSize tests
  // ============================================================

  test("SocketAddress.sockAddrSize returns correct size for V4"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.Wildcard)
    assertEquals(addr.sockAddrSize.toLong, sizeof[sockaddr_in].toLong)

  test("SocketAddress.sockAddrSize returns correct size for V6"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.Wildcard)
    assertEquals(addr.sockAddrSize.toLong, sizeof[sockaddr_in6].toLong)

  // ============================================================
  // native.fromSockAddr tests
  // ============================================================

  test("native.fromSockAddr parses sockaddr_in correctly"):
    Zone.acquire { implicit z =>
      val sin = alloc[sockaddr_in]()
      sin.sin_family = AF_INET.toUShort
      sin.sin_port = htons(3000.toUShort)
      sin.sin_addr.s_addr = htonl(0xC0A80101.toUInt) // 192.168.1.1

      val result = fromSockAddr(sin.asInstanceOf[Ptr[sockaddr]])
      assert(result.isRight)
      result.foreach { addr =>
        assert(addr.isV4)
        assertEquals(addr.port.value, 3000)
        assertEquals(addr.toIpv4.get.show, "192.168.1.1")
      }
    }

  test("native.fromSockAddr parses sockaddr_in6 correctly"):
    Zone.acquire { implicit z =>
      val sin6 = alloc[sockaddr_in6]()
      sin6.sin6_family = AF_INET6.toUShort
      sin6.sin6_port = htons(8080.toUShort)
      sin6.sin6_flowinfo = htonl(100.toUInt)
      sin6.sin6_scope_id = htonl(2.toUInt)
      // Write ::1 to sin6_addr via raw pointer access
      val addrPtr = sin6.asInstanceOf[Ptr[Byte]] + 8 // SIN6_ADDR_OFFSET
      (0 until 15).foreach(i => addrPtr(i) = 0.toByte)
      addrPtr(15) = 1.toByte

      val result = fromSockAddr(sin6.asInstanceOf[Ptr[sockaddr]])
      assert(result.isRight)
      result.foreach { addr =>
        assert(addr.isV6)
        assertEquals(addr.port.value, 8080)
        addr match
          case SocketAddress.V6(ip, _, fi, sid) =>
            assertEquals(ip.show, "::1")
            assertEquals(fi.value, 100)
            assertEquals(sid.value, 2)
          case _ => fail("Expected V6")
      }
    }

  test("native.fromSockAddr returns Left for unknown address family"):
    Zone.acquire { implicit z =>
      val sockaddr = alloc[sockaddr]()
      sockaddr.sa_family = 99.toUShort // Invalid family

      val result = fromSockAddr(sockaddr)
      assert(result.isLeft)
    }

  // ============================================================
  // Full roundtrip tests
  // ============================================================

  test("IPv4 sockaddr roundtrip"):
    Zone.acquire { implicit z =>
      val original = SocketAddress.v4(
        Ipv4Address.fromString("10.20.30.40").get,
        Port.fromInt(5432).toOption.get
      )

      val sockaddr = original.toSockAddr
      val result = fromSockAddr(sockaddr)

      assert(result.isRight)
      result.foreach { parsed =>
        assertEquals(parsed.show, original.show)
        assertEquals(parsed.port, original.port)
        assertEquals(parsed.toIpv4.get.show, "10.20.30.40")
      }
    }

  test("IPv6 sockaddr roundtrip"):
    Zone.acquire { implicit z =>
      val flowInfo = FlowInfo(0x12345)
      val scopeId = ScopeId(3)
      val original = SocketAddress.v6(
        Ipv6Address.fromString("2001:db8::1").get,
        Port.fromInt(443).toOption.get,
        flowInfo,
        scopeId
      )

      val sockaddr = original.toSockAddr
      val result = fromSockAddr(sockaddr)

      assert(result.isRight)
      result.foreach { parsed =>
        assertEquals(parsed.port, original.port)
        parsed match
          case SocketAddress.V6(ip, p, fi, sid) =>
            assertEquals(ip.show, "2001:db8::1")
            assertEquals(fi.value, flowInfo.value)
            assertEquals(sid.value, scopeId.value)
          case _ => fail("Expected V6")
      }
    }

  test("Loopback sockaddr roundtrip"):
    Zone.acquire { implicit z =>
      val v4 = SocketAddress.localhost(Port.fromInt(80).toOption.get)
      val v4Result = fromSockAddr(v4.toSockAddr)
      assert(v4Result.isRight)
      assertEquals(v4Result.toOption.get.show, "127.0.0.1:80")

      val v6 = SocketAddress.localhost6(Port.fromInt(443).toOption.get)
      val v6Result = fromSockAddr(v6.toSockAddr)
      assert(v6Result.isRight)
      assertEquals(v6Result.toOption.get.show, "[::1]:443")
    }

  test("Wildcard sockaddr roundtrip"):
    Zone.acquire { implicit z =>
      val v4 = SocketAddress.any(Port.fromInt(0).toOption.get)
      val v4Result = fromSockAddr(v4.toSockAddr)
      assert(v4Result.isRight)
      assertEquals(v4Result.toOption.get.show, "0.0.0.0:0")

      val v6 = SocketAddress.any6(Port.fromInt(0).toOption.get)
      val v6Result = fromSockAddr(v6.toSockAddr)
      assert(v6Result.isRight)
      assertEquals(v6Result.toOption.get.show, "[::]:0")
    }

  test("Various IPv4 addresses roundtrip"):
    Zone.acquire { implicit z =>
      val addresses = List(
        "192.168.1.1",
        "10.0.0.1",
        "172.16.0.1",
        "255.255.255.255",
        "1.2.3.4"
      )

      addresses.foreach { addrStr =>
        val original = SocketAddress.v4(
          Ipv4Address.fromString(addrStr).get,
          Port.fromInt(8080).toOption.get
        )
        val result = fromSockAddr(original.toSockAddr)
        assert(result.isRight, s"Failed for $addrStr")
        assertEquals(result.toOption.get.toIpv4.get.show, addrStr)
      }
    }

  test("Various IPv6 addresses roundtrip"):
    Zone.acquire { implicit z =>
      val addresses = List(
        "::",
        "::1",
        "fe80::1",
        "ff02::1",
        "2001:db8::1"
      )

      addresses.foreach { addrStr =>
        val original = SocketAddress.v6(
          Ipv6Address.fromString(addrStr).get,
          Port.fromInt(443).toOption.get
        )
        val result = fromSockAddr(original.toSockAddr)
        assert(result.isRight, s"Failed for $addrStr")
        assertEquals(result.toOption.get.toIpv6.get.show, addrStr)
      }
    }

end NativePlatformSpec
