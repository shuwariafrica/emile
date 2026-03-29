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
package emile.cats

import cats.effect.IO

import boilerplate.effect.Eff

import emile.EmileError
import emile.ipa.Ipv4Address
import emile.ipa.Ipv6Address
import emile.ipa.Port
import emile.ipa.SocketAddress

/** Tests for DnsResolver - async DNS resolution. */
class DnsResolverSuite extends EmileSuite:

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("DnsResolver.resolve resolves localhost") {
    runEff {
      for addresses <- DnsResolver.resolve("localhost", "80")
      yield assert(addresses.nonEmpty, "localhost should resolve to at least one address")
    }
  }

  test("DnsResolver.resolve with numeric IPv4 returns that IP") {
    runEff {
      for addresses <- DnsResolver.resolve("127.0.0.1", "443")
      yield
        assert(addresses.nonEmpty, "127.0.0.1 should resolve")
        addresses.foreach {
          case SocketAddress.V4(addr, port) =>
            assertEquals(addr, Ipv4Address.Loopback)
            assertEquals(port, Port.wrap(443))
          case _ => fail("Expected IPv4 address")
        }
    }
  }

  test("DnsResolver.resolve with numeric IPv6 returns that IP") {
    runEff {
      for addresses <- DnsResolver.resolve("::1", 8080)
      yield
        assert(addresses.nonEmpty, "::1 should resolve")
        addresses.foreach {
          case SocketAddress.V6(addr, port, _, _) =>
            assertEquals(addr, Ipv6Address.Loopback)
            assertEquals(port, Port.wrap(8080))
          case _ => fail("Expected IPv6 address")
        }
    }
  }

  test("DnsResolver.resolve with port number works") {
    runEff {
      for addresses <- DnsResolver.resolve("localhost", 8080)
      yield
        assert(addresses.nonEmpty, "localhost should resolve")
        addresses.foreach { addr =>
          assertEquals(addr.port, Port.wrap(8080))
        }
    }
  }

  test("DnsResolver.resolve with service name works") {
    runEff {
      for addresses <- DnsResolver.resolve("localhost", "http")
      yield
        assert(addresses.nonEmpty, "localhost with http service should resolve")
        addresses.foreach { addr =>
          assertEquals(addr.port, Port.wrap(80))
        }
    }
  }

  test("DnsResolver.resolve fails for invalid hostname") {
    runEff {
      DnsResolver
        .resolve("this.hostname.definitely.does.not.exist.invalid", "80")
        .map(_ => fail("Should have failed"))
        .catchAll { (_: EmileError) =>
          Eff.succeed[IO, EmileError, Unit](()) // Expected: DNS resolution failure
        }
    }
  }

  test("DnsResolver.resolve fails for empty hostname") {
    runEff {
      DnsResolver
        .resolve("", "80")
        .map(_ => fail("Should have failed"))
        .catchAll { (_: EmileError) =>
          Eff.succeed[IO, EmileError, Unit](())
        }
    }
  }

end DnsResolverSuite
