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

import scala.concurrent.duration.DurationInt

/** Per-socket TCP tuning, applied at bind / connect / accept time. Constructed from the presets and
  * `copy` on [[TcpOptions$ TcpOptions]].
  *
  * @param noDelay enable `TCP_NODELAY` to disable Nagle's algorithm
  * @param simultaneousAccepts whether to use libuv's simultaneous-accept mode on the listener (load
  *   distribution / throughput trade-off; no-op on Unix - libuv only consults it on Windows)
  * @param reusePort enable `SO_REUSEPORT` for the listener - lets multiple binders share a port
  * @param ipv6Only disable IPv6 dual-stack on an IPv6 bind
  * @param listenBacklog the `listen(2)` backlog
  */
final case class TcpOptions(
  noDelay: Boolean,
  keepAlive: Option[TcpKeepAlive],
  simultaneousAccepts: Boolean,
  reusePort: Boolean,
  ipv6Only: Boolean,
  listenBacklog: Int
) derives CanEqual

/** Presets for [[TcpOptions]]. */
object TcpOptions:

  /** Conservative defaults: Nagle on, no keep-alive, simultaneous-accept on, no port reuse, IPv6
    * dual-stack on, listen backlog 1024.
    */
  val default: TcpOptions = TcpOptions(
    noDelay = false,
    keepAlive = None,
    simultaneousAccepts = true,
    reusePort = false,
    ipv6Only = false,
    listenBacklog = 1024
  )

  /** Defaults plus `TCP_NODELAY` - the low-latency profile. */
  val lowLatency: TcpOptions = default.copy(noDelay = true)

  /** Defaults plus `TCP_NODELAY`, `SO_REUSEPORT`, and a 60-second keep-alive - the server profile. */
  val server: TcpOptions = default.copy(
    noDelay = true,
    reusePort = true,
    keepAlive = Some(TcpKeepAlive.simple(60.seconds))
  )

end TcpOptions
