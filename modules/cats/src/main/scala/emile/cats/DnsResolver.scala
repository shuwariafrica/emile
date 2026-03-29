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

import boilerplate.effect.*

import emile.Dns
import emile.EmileError
import emile.ipa.SocketAddress

/** Async DNS resolution via libuv's `uv_getaddrinfo`.
  *
  * {{{
  * DnsResolver.resolve("example.com", "80").rethrow
  * }}}
  */
object DnsResolver:

  /** Resolve a hostname to a list of socket addresses.
    *
    * This is an async operation that uses libuv's uv_getaddrinfo under the hood. The resolution may
    * involve network I/O and could take significant time.
    *
    * @param node The hostname to resolve (e.g., "example.com", "localhost")
    * @param service The service name or port number (e.g., "http", "80", "443")
    * @return Eff containing the list of resolved socket addresses
    */
  def resolve(node: String, service: String): Eff[IO, EmileError, List[SocketAddress]] =
    EffAsync.asyncWithPendingCancellable[List[SocketAddress]] { (loop, complete) =>
      Dns.getAddrInfo(loop, node, service) { result =>
        complete(result)
      } match
        case Right(_) =>
          // Request submitted successfully, cancellation not supported by libuv
          Eff.succeed[IO, EmileError, Option[Eff[IO, EmileError, Unit]]](None)
        case Left(err) =>
          // Request initiation failed
          complete(Left(err))
          Eff.succeed[IO, EmileError, Option[Eff[IO, EmileError, Unit]]](None)
    }

  /** Resolve a hostname with address family and socket type hints.
    *
    * Allows specifying address family hints to control the types of addresses returned (IPv4 only,
    * IPv6 only, etc.) and socket type (TCP/UDP).
    *
    * @param node The hostname to resolve
    * @param service The service name or port number
    * @param family Address family hint (use Dns.AF_INET, Dns.AF_INET6, or Dns.AF_UNSPEC)
    * @param socktype Socket type hint (use Dns.SOCK_STREAM for TCP, Dns.SOCK_DGRAM for UDP)
    * @return Eff containing the list of resolved socket addresses
    */
  def resolveWithHints(node: String, service: String, family: Int, socktype: Int): Eff[IO, EmileError, List[SocketAddress]] =
    EffAsync.asyncWithPendingCancellable[List[SocketAddress]] { (loop, complete) =>
      Dns.getAddrInfoWithHints(loop, node, service, family, socktype) { result =>
        complete(result)
      } match
        case Right(_) =>
          Eff.succeed[IO, EmileError, Option[Eff[IO, EmileError, Unit]]](None)
        case Left(e) =>
          complete(Left(e))
          Eff.succeed[IO, EmileError, Option[Eff[IO, EmileError, Unit]]](None)
    }

  /** Resolve a hostname with port specified as integer.
    *
    * Convenience overload that converts the port to a service string.
    *
    * @param node The hostname to resolve
    * @param port The port number
    * @return Eff containing the list of resolved socket addresses
    */
  def resolve(node: String, port: Int): Eff[IO, EmileError, List[SocketAddress]] =
    resolve(node, port.toString)

  /** Resolve a hostname without service/port specification.
    *
    * Returns addresses without port information (port 0).
    *
    * @param node The hostname to resolve
    * @return Eff containing the list of resolved socket addresses
    */
  def resolve(node: String): Eff[IO, EmileError, List[SocketAddress]] =
    resolve(node, "")
end DnsResolver
