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

import scala.concurrent.duration.FiniteDuration

/** TCP keep-alive parameters - the three `uv_tcp_keepalive_ex` knobs together. Constructed through
  * [[TCPKeepAlive$ TCPKeepAlive]].
  *
  * @param idle the `TCP_KEEPIDLE` window - the connection's idle period before the first probe
  * @param interval the `TCP_KEEPINTVL` spacing between probes
  * @param count the `TCP_KEEPCNT` cap on unanswered probes before the connection is dropped
  */
final case class TCPKeepAlive(
  idle: FiniteDuration,
  interval: FiniteDuration,
  count: Int
) derives CanEqual

/** Presets and constants for [[TCPKeepAlive]]. */
object TCPKeepAlive:

  /** The default unanswered-probe cap - matches libuv's pre-`_ex` `uv_tcp_keepalive` (9 probes
    * after the initial delay).
    */
  inline val DefaultProbeCount = 9

  /** A keep-alive whose idle and probe-interval windows are both `after`, with the default probe
    * count.
    */
  def simple(after: FiniteDuration): TCPKeepAlive = TCPKeepAlive(after, after, DefaultProbeCount)
