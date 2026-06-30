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
package emile

import com.comcast.ip4s.Hostname
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port

/** Covers [[DNS]]: forward and reverse resolution of the loopback host, which the platform resolver
  * answers locally without a network.
  */
final class DNSSpec extends EmileSuite:

  private val localhost: Hostname = Hostname.fromString("localhost").get
  private val loopback: IpAddress = IpAddress.fromString("127.0.0.1").get

  test("resolve(host, port) returns the loopback address") {
    DNS
      .resolve(localhost, Port.fromInt(0).get)
      .absolve
      .map(addresses => assert(addresses.toList.exists(_.host.isLoopback)))
  }

  test("resolve(host) returns the loopback address") {
    DNS.resolve(localhost).absolve.map(addresses => assert(addresses.toList.exists(_.isLoopback)))
  }

  test("reverse resolves the loopback address to a host name") {
    DNS.reverse(loopback).absolve.map(name => assert(name.toString.nonEmpty))
  }

  test("resolve of a nonexistent host fails with UnknownHost") {
    val invalid = Hostname.fromString("nonexistent.invalid").get
    DNS.resolve(invalid, Port.fromInt(0).get).either.map {
      case Left(EmileError.DNS.UnknownHost(host)) => assertEquals(host, "nonexistent.invalid")
      case other => fail(s"expected UnknownHost, got: $other")
    }
  }

end DNSSpec
