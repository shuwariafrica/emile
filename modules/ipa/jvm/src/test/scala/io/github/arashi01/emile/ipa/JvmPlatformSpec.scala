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

import java.net.Inet4Address
import java.net.Inet6Address
import java.net.InetAddress
import java.net.InetSocketAddress

/**
 * Platform-specific tests for JVM InetAddress conversions.
 *
 * These tests exercise actual JVM networking classes and verify correct
 * conversion between emile-ipa types and java.net types.
 *
 * Tests cover:
 * - Ipv4Address.toInetAddress: conversion to Inet4Address
 * - Ipv6Address.toInetAddress: conversion to Inet6Address
 * - SocketAddress.toInetSocketAddress: conversion to InetSocketAddress
 * - jvm.fromInet4Address: parsing from Inet4Address
 * - jvm.fromInet6Address: parsing from Inet6Address
 * - jvm.fromInetSocketAddress: parsing from InetSocketAddress
 * - Full roundtrip tests
 */
class JvmPlatformSpec extends FunSuite:

  // ============================================================
  // Ipv4Address.toInetAddress tests
  // ============================================================

  test("Ipv4Address.toInetAddress creates Inet4Address"):
    val addr = Ipv4Address.fromString("192.168.1.1").get
    val inet = addr.toInetAddress
    assert(inet.isInstanceOf[Inet4Address])
    assertEquals(inet.getHostAddress.nn, "192.168.1.1")

  test("Ipv4Address.toInetAddress for loopback"):
    val inet = Ipv4Address.Loopback.toInetAddress
    assert(inet.isLoopbackAddress)
    assertEquals(inet.getHostAddress.nn, "127.0.0.1")

  test("Ipv4Address.toInetAddress for wildcard"):
    val inet = Ipv4Address.Wildcard.toInetAddress
    assert(inet.isAnyLocalAddress)
    assertEquals(inet.getHostAddress.nn, "0.0.0.0")

  test("Ipv4Address.toInetAddress preserves bytes"):
    val addr = Ipv4Address.fromString("10.20.30.40").get
    val inet = addr.toInetAddress
    val bytes = inet.getAddress.nn
    assertEquals(bytes.length, 4)
    assertEquals(bytes(0) & 0xff, 10)
    assertEquals(bytes(1) & 0xff, 20)
    assertEquals(bytes(2) & 0xff, 30)
    assertEquals(bytes(3) & 0xff, 40)

  // ============================================================
  // Ipv6Address.toInetAddress tests
  // ============================================================

  test("Ipv6Address.toInetAddress creates Inet6Address"):
    val addr = Ipv6Address.fromString("2001:db8::1").get
    val inet = addr.toInetAddress
    assert(inet.isInstanceOf[Inet6Address])

  test("Ipv6Address.toInetAddress for loopback"):
    val inet = Ipv6Address.Loopback.toInetAddress
    assert(inet.isLoopbackAddress)

  test("Ipv6Address.toInetAddress for wildcard"):
    val inet = Ipv6Address.Wildcard.toInetAddress
    assert(inet.isAnyLocalAddress)

  test("Ipv6Address.toInetAddress preserves bytes"):
    val addr = Ipv6Address.fromString("::1").get
    val inet = addr.toInetAddress
    val bytes = inet.getAddress.nn
    assertEquals(bytes.length, 16)
    (0 until 15).foreach(i => assertEquals(bytes(i).toInt, 0))
    assertEquals(bytes(15).toInt, 1)

  test("Ipv6Address.toInetAddress for link-local"):
    val addr = Ipv6Address.fromString("fe80::1").get
    val inet = addr.toInetAddress
    assert(inet.isLinkLocalAddress)

  test("Ipv6Address.toInetAddress for multicast"):
    val addr = Ipv6Address.fromString("ff02::1").get
    val inet = addr.toInetAddress
    assert(inet.isMulticastAddress)

  // ============================================================
  // SocketAddress.toInetSocketAddress tests
  // ============================================================

  test("SocketAddress.toInetSocketAddress for V4"):
    val addr = SocketAddress.v4(Ipv4Address.Loopback, Port.fromInt(8080).toOption.get)
    val inetSock = addr.toInetSocketAddress
    assertEquals(inetSock.getPort, 8080)
    assertEquals(inetSock.getAddress.nn.getHostAddress.nn, "127.0.0.1")

  test("SocketAddress.toInetSocketAddress for V6"):
    val addr = SocketAddress.v6(Ipv6Address.Loopback, Port.fromInt(443).toOption.get)
    val inetSock = addr.toInetSocketAddress
    assertEquals(inetSock.getPort, 443)
    assert(inetSock.getAddress.nn.isLoopbackAddress)

  test("SocketAddress.toInetSocketAddress preserves port"):
    val ports = List(0, 80, 443, 8080, 65535)
    ports.foreach { p =>
      val addr = SocketAddress.localhost(Port.fromInt(p).toOption.get)
      assertEquals(addr.toInetSocketAddress.getPort, p)
    }

  // ============================================================
  // jvm.fromInet4Address tests
  // ============================================================

  test("jvm.fromInet4Address parses Inet4Address"):
    val inet = InetAddress.getByName("192.168.1.1").nn.asInstanceOf[Inet4Address]
    val addr = fromInet4Address(inet)
    assertEquals(addr.show, "192.168.1.1")

  test("jvm.fromInet4Address parses loopback"):
    val inet = InetAddress.getByName("127.0.0.1").nn.asInstanceOf[Inet4Address]
    val addr = fromInet4Address(inet)
    assertEquals(addr, Ipv4Address.Loopback)

  test("jvm.fromInet4Address preserves octets"):
    val inet = InetAddress.getByName("10.20.30.40").nn.asInstanceOf[Inet4Address]
    val addr = fromInet4Address(inet)
    assertEquals(addr.octet1, 10)
    assertEquals(addr.octet2, 20)
    assertEquals(addr.octet3, 30)
    assertEquals(addr.octet4, 40)

  // ============================================================
  // jvm.fromInet6Address tests
  // ============================================================

  test("jvm.fromInet6Address parses Inet6Address"):
    val inet = InetAddress.getByName("2001:db8::1").nn.asInstanceOf[Inet6Address]
    val addr = fromInet6Address(inet)
    assertEquals(addr.show, "2001:db8::1")

  test("jvm.fromInet6Address parses loopback"):
    val inet = InetAddress.getByName("::1").nn.asInstanceOf[Inet6Address]
    val addr = fromInet6Address(inet)
    assertEquals(addr, Ipv6Address.Loopback)

  test("jvm.fromInet6Address parses wildcard"):
    val inet = InetAddress.getByName("::").nn.asInstanceOf[Inet6Address]
    val addr = fromInet6Address(inet)
    assertEquals(addr, Ipv6Address.Wildcard)

  // ============================================================
  // jvm.fromInetSocketAddress tests
  // ============================================================

  test("jvm.fromInetSocketAddress parses IPv4 socket address"):
    val inetSock = new InetSocketAddress("192.168.1.1", 8080)
    val result = fromInetSocketAddress(inetSock)
    assert(result.isRight)
    result.foreach { addr =>
      assert(addr.isV4)
      assertEquals(addr.port.value, 8080)
      assertEquals(addr.toIpv4.get.show, "192.168.1.1")
    }

  test("jvm.fromInetSocketAddress parses IPv6 socket address"):
    val inetSock = new InetSocketAddress("::1", 443)
    val result = fromInetSocketAddress(inetSock)
    assert(result.isRight)
    result.foreach { addr =>
      assert(addr.isV6)
      assertEquals(addr.port.value, 443)
    }

  test("jvm.fromInetSocketAddress handles port 0"):
    val inetSock = new InetSocketAddress("127.0.0.1", 0)
    val result = fromInetSocketAddress(inetSock)
    assert(result.isRight)
    assertEquals(result.toOption.get.port.value, 0)

  test("jvm.fromInetSocketAddress handles max port"):
    val inetSock = new InetSocketAddress("127.0.0.1", 65535)
    val result = fromInetSocketAddress(inetSock)
    assert(result.isRight)
    assertEquals(result.toOption.get.port.value, 65535)

  // ============================================================
  // Full roundtrip tests - IPv4
  // ============================================================

  test("IPv4 roundtrip via InetAddress"):
    val original = Ipv4Address.fromString("172.16.0.100").get
    val inet = original.toInetAddress
    val roundtripped = fromInet4Address(inet)
    assertEquals(roundtripped, original)

  test("IPv4 roundtrip for boundary addresses"):
    val addresses = List(
      Ipv4Address.Wildcard,
      Ipv4Address.Loopback,
      Ipv4Address.Broadcast,
      Ipv4Address.fromString("255.254.253.252").get
    )

    addresses.foreach { original =>
      val inet = original.toInetAddress
      val roundtripped = fromInet4Address(inet)
      assertEquals(roundtripped.show, original.show)
    }

  test("IPv4 SocketAddress roundtrip via InetSocketAddress"):
    val original = SocketAddress.v4(
      Ipv4Address.fromString("10.0.0.1").get,
      Port.fromInt(5432).toOption.get
    )
    val inetSock = original.toInetSocketAddress
    val result = fromInetSocketAddress(inetSock)
    assert(result.isRight)
    assertEquals(result.toOption.get.show, original.show)

  // ============================================================
  // Full roundtrip tests - IPv6
  // ============================================================

  test("IPv6 roundtrip via InetAddress"):
    val original = Ipv6Address.fromString("2001:db8::abcd:1234").get
    val inet = original.toInetAddress
    val roundtripped = fromInet6Address(inet)
    assertEquals(roundtripped.highBits, original.highBits)
    assertEquals(roundtripped.lowBits, original.lowBits)

  test("IPv6 roundtrip for boundary addresses"):
    val addresses = List(
      Ipv6Address.Wildcard,
      Ipv6Address.Loopback,
      Ipv6Address.fromString("fe80::1").get,
      Ipv6Address.fromString("ff02::1").get,
      Ipv6Address.fromLongs(-1L, -1L) // All ones
    )

    addresses.foreach { original =>
      val inet = original.toInetAddress
      val roundtripped = fromInet6Address(inet)
      assertEquals(roundtripped.show, original.show)
    }

  test("IPv6 SocketAddress roundtrip via InetSocketAddress"):
    val original = SocketAddress.v6(
      Ipv6Address.fromString("fe80::1").get,
      Port.fromInt(443).toOption.get
    )
    val inetSock = original.toInetSocketAddress
    val result = fromInetSocketAddress(inetSock)
    assert(result.isRight)
    // Note: FlowInfo and ScopeId are not preserved through InetSocketAddress
    assertEquals(result.toOption.get.port, original.port)

  // ============================================================
  // Various addresses roundtrip
  // ============================================================

  test("Various IPv4 addresses roundtrip"):
    val addresses = List(
      "0.0.0.0",
      "1.2.3.4",
      "10.0.0.1",
      "127.0.0.1",
      "169.254.1.1",
      "172.16.0.1",
      "192.168.0.1",
      "224.0.0.1",
      "255.255.255.255"
    )

    addresses.foreach { addrStr =>
      val original = Ipv4Address.fromString(addrStr).get
      val roundtripped = fromInet4Address(original.toInetAddress)
      assertEquals(roundtripped.show, addrStr, s"Failed for $addrStr")
    }

  test("Various IPv6 addresses roundtrip"):
    val addresses = List(
      "::",
      "::1",
      "fe80::1",
      "ff02::1",
      "2001:db8::1",
      "2001:db8:85a3::8a2e:370:7334"
    )

    addresses.foreach { addrStr =>
      val original = Ipv6Address.fromString(addrStr).get
      val roundtripped = fromInet6Address(original.toInetAddress)
      assertEquals(roundtripped.show, addrStr, s"Failed for $addrStr")
    }

  // ============================================================
  // Consistency tests with JVM networking behavior
  // ============================================================

  test("toInetAddress.isLoopbackAddress matches our isLoopback"):
    assert(Ipv4Address.Loopback.toInetAddress.isLoopbackAddress)
    assert(Ipv6Address.Loopback.toInetAddress.isLoopbackAddress)

  test("toInetAddress.isAnyLocalAddress matches our isWildcard"):
    assert(Ipv4Address.Wildcard.toInetAddress.isAnyLocalAddress)
    assert(Ipv6Address.Wildcard.toInetAddress.isAnyLocalAddress)

  test("toInetAddress.isLinkLocalAddress for IPv4 link-local"):
    val linkLocal = Ipv4Address.fromString("169.254.1.1").get
    assert(linkLocal.toInetAddress.isLinkLocalAddress)
    assert(linkLocal.isLinkLocal)

  test("toInetAddress.isLinkLocalAddress for IPv6 link-local"):
    val linkLocal = Ipv6Address.fromString("fe80::1").get
    assert(linkLocal.toInetAddress.isLinkLocalAddress)
    assert(linkLocal.isLinkLocal)

  test("toInetAddress.isMulticastAddress for IPv4 multicast"):
    val multicast = Ipv4Address.fromString("224.0.0.1").get
    assert(multicast.toInetAddress.isMulticastAddress)
    assert(multicast.isMulticast)

  test("toInetAddress.isMulticastAddress for IPv6 multicast"):
    val multicast = Ipv6Address.fromString("ff02::1").get
    assert(multicast.toInetAddress.isMulticastAddress)
    assert(multicast.isMulticast)

end JvmPlatformSpec
