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

/** Options for a [[UDPSocket]], applied during [[UDP.bind]]. Construct from the presets and `copy`
  * on [[UDPOptions$ UDPOptions]].
  *
  * @param reusePort enable `SO_REUSEPORT` (the load-balancing flag) so several sockets can share
  *   the address and port and the kernel distributes datagrams across them; a platform without it
  *   fails the bind with a typed `ENOTSUP`
  * @param ipv6Only disable IPv6 dual-stack on an IPv6 bind
  * @param batchedReceive drain the socket with `recvmmsg`, gathering several datagrams per syscall -
  *   the high-throughput receive path; delivery to the consumer is per datagram either way
  */
final case class UDPOptions(reusePort: Boolean, ipv6Only: Boolean, batchedReceive: Boolean) derives CanEqual

/** Presets for [[UDPOptions]]. */
object UDPOptions:

  /** Conservative defaults: no port reuse, IPv6 dual-stack on, single-datagram receive. */
  val default: UDPOptions = UDPOptions(reusePort = false, ipv6Only = false, batchedReceive = false)

  /** Defaults plus batched `recvmmsg` receive - the high-throughput profile. */
  val highThroughput: UDPOptions = default.copy(batchedReceive = true)
