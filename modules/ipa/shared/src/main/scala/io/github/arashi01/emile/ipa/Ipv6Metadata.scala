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

/**
 * IPv6 flow information (traffic class and flow label).
 *
 * This 32-bit field contains:
 *   - Traffic class (8 bits): Similar to IPv4 TOS/DSCP
 *   - Flow label (20 bits): For QoS handling
 *
 * Used in IPv6 socket addresses for advanced traffic handling.
 */
opaque type FlowInfo = Int

object FlowInfo:
  given CanEqual[FlowInfo, FlowInfo] = CanEqual.derived
  given Ordering[FlowInfo]           = Ordering.Int

  /** Default flow info (zero). */
  val Default: FlowInfo = 0

  /** Construct from raw 32-bit value. */
  inline def apply(value: Int): FlowInfo = value

  extension (fi: FlowInfo)
    /** Get the underlying 32-bit value. */
    inline def value: Int = fi

    /** Get the traffic class (upper 8 bits of the 28 significant bits). */
    def trafficClass: Int = (fi >>> 20) & 0xff

    /** Get the flow label (lower 20 bits). */
    def flowLabel: Int = fi & 0xfffff

end FlowInfo

/**
 * IPv6 scope identifier for link-local addresses.
 *
 * This identifies the network interface for link-local destinations. On most
 * systems, the scope ID corresponds to the interface index.
 *
 * A scope ID of 0 means "unspecified" and is appropriate for non-link-local
 * addresses.
 */
opaque type ScopeId = Int

object ScopeId:
  given CanEqual[ScopeId, ScopeId] = CanEqual.derived
  given Ordering[ScopeId]          = Ordering.Int

  /** Default scope ID (zero - unspecified). */
  val Default: ScopeId = 0

  /** Construct from raw 32-bit value. */
  inline def apply(value: Int): ScopeId = value

  extension (sid: ScopeId)
    /** Get the underlying 32-bit value. */
    inline def value: Int = sid

    /** True if this is the default (unspecified) scope. */
    def isDefault: Boolean = sid == 0

end ScopeId
