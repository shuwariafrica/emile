/*
 * Copyright 2025, 2026 Ali Rashid
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
package emile.unsafe

import scala.scalanative.unsafe.stackalloc

import com.comcast.ip4s.*

import munit.FunSuite

/** Covers [[SockAddr]]'s IPv6 zone round-trip: a link-local address keeps its interface-name zone
  * through a write/read cycle (the zone travels as the numeric interface index in `sin6_scope_id`),
  * an address without a zone gains none, and IPv4 is unaffected. Grounded on the `lo` loopback
  * interface, always present.
  */
final class SockAddrSpec extends FunSuite:

  private def roundTrip(address: SocketAddress[IpAddress]): Option[SocketAddress[IpAddress]] =
    val storage = stackalloc[Byte](SockAddr.storageSize)
    SockAddr.write(address, storage)
    SockAddr.read(storage)

  test("an IPv6 zone id survives a sockaddr round-trip") {
    val address = SocketAddress(ipv6"fe80::1".withScopeId("lo"), port"443")
    assertEquals(roundTrip(address), Some(address): Option[SocketAddress[IpAddress]])
  }

  test("an IPv6 address without a zone round-trips without one") {
    val address = SocketAddress(ipv6"fe80::1", port"443")
    assertEquals(roundTrip(address), Some(address): Option[SocketAddress[IpAddress]])
  }

  test("an IPv4 address round-trips unchanged") {
    val address = SocketAddress(ipv4"127.0.0.1", port"80")
    assertEquals(roundTrip(address), Some(address): Option[SocketAddress[IpAddress]])
  }
end SockAddrSpec
