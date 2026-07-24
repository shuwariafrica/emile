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

/** The Don't-Fragment / Path MTU Discovery policy on a [[UDPChannel]] - the Linux `IP_MTU_DISCOVER`
  * modes. Set at bind through [[ChannelConfig]] and switched at runtime with
  * [[UDPChannel.setPmtud]] as a DPLPMTUD state machine drives it. Variants are on
  * [[PmtudMode$ PmtudMode]].
  */
enum PmtudMode derives CanEqual:
  /** Never set DF; the kernel fragments an oversized datagram. */
  case Dont

  /** Set DF per the route's PMTU setting, fragmenting otherwise. */
  case Want

  /** Always set DF and enforce the discovered PMTU - an oversized send fails with
    * [[EmileError.IO.MessageTooLarge]] before transmission.
    */
  case Do

  /** Set DF but ignore the discovered PMTU, so an oversized probe is sent rather than rejected -
    * the RFC 8899 probing mode (`IP_PMTUDISC_PROBE`).
    */
  case Probe
end PmtudMode

/** Options for a [[UDPChannel]], applied during [[UDP.channel]]. ECN reception is always on (it is
  * cheap and the transport profile requires it), so it is not a field here. Construct from the
  * presets and `copy` on [[ChannelConfig$ ChannelConfig]].
  *
  * @param reusePort enable `SO_REUSEPORT` so several channels can share the address and port, the
  *   kernel distributing datagrams across them - the per-worker receive-scaling profile
  * @param pktinfo deliver each datagram's local address and arrival interface, and allow
  *   per-datagram source selection on send
  * @param gro coalesce received datagrams (`UDP_GRO`); the channel splits a coalesced buffer back
  *   into per-datagram sub-slices, so the consumer still sees datagrams
  * @param pmtud the Don't-Fragment / PMTU policy
  */
final case class ChannelConfig(reusePort: Boolean, pktinfo: Boolean, gro: Boolean, pmtud: PmtudMode) derives CanEqual

/** Presets for [[ChannelConfig]]. */
object ChannelConfig:

  /** Conservative defaults: no port reuse, no PKTINFO, no GRO, and no DF ([[PmtudMode.Dont]]). */
  val default: ChannelConfig = ChannelConfig(reusePort = false, pktinfo = false, gro = false, pmtud = PmtudMode.Dont)

  /** The QUIC substrate: PKTINFO and GRO on, and [[PmtudMode.Do]] so an oversized send is rejected
    * locally with [[EmileError.IO.MessageTooLarge]].
    */
  val quic: ChannelConfig = ChannelConfig(reusePort = false, pktinfo = true, gro = true, pmtud = PmtudMode.Do)
