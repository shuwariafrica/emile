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
 * Platform-specific tests for Scala.js.
 *
 * Currently the JS platform has no platform-specific extensions.
 * These tests verify that the shared code works correctly on JS.
 *
 * Note: The shared tests run on all platforms. These tests are for any
 * JS-specific behavior or to verify JS-specific code paths.
 */
class JsPlatformSpec extends FunSuite:

  // ============================================================
  // Basic verification that shared types work on JS
  // ============================================================

  test("Port works on JS platform"):
    val port = Port.fromInt(8080).toOption.get
    assertEquals(port.value, 8080)
    assertEquals(port.show, "8080")

  test("Ipv4Address works on JS platform"):
    val addr = Ipv4Address.fromString("192.168.1.1").get
    assertEquals(addr.show, "192.168.1.1")
    assertEquals(addr.octet1, 192)
    assertEquals(addr.octet2, 168)
    assertEquals(addr.octet3, 1)
    assertEquals(addr.octet4, 1)

  test("Ipv6Address works on JS platform"):
    val addr = Ipv6Address.fromString("2001:db8::1").get
    assertEquals(addr.show, "2001:db8::1")

  test("SocketAddress works on JS platform"):
    val addr = SocketAddress.localhost(Port.fromInt(3000).toOption.get)
    assertEquals(addr.show, "127.0.0.1:3000")

  test("String interpolators work on JS platform"):
    // Note: These are compile-time, so this mainly tests that they link
    import literals.*

    val port = port"8080"
    assertEquals(port.value, 8080)

    val v4 = ipv4"127.0.0.1"
    assertEquals(v4.show, "127.0.0.1")

    val v6 = ipv6"::1"
    assertEquals(v6.show, "::1")

end JsPlatformSpec
