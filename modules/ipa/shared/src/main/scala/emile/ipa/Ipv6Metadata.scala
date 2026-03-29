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
package emile.ipa

import boilerplate.*

/** IPv6 flow information (traffic class and flow label).
  *
  * This 32-bit field contains:
  *   - Traffic class (8 bits): Similar to IPv4 TOS/DSCP
  *   - Flow label (20 bits): For QoS handling
  *
  * Instances may be constructed via [[FlowInfo$ FlowInfo]].
  */
opaque type FlowInfo = Int

/** Provides factories and extension syntax for [[FlowInfo]]. */
object FlowInfo extends OpaqueType[FlowInfo, Int], OpaqueType.Eq[FlowInfo]:
  type Error = Nothing

  given Ordering[FlowInfo] = Ordering.Int

  inline def wrap(value: Int): FlowInfo = value
  inline def unwrap(fi: FlowInfo): Int = fi
  protected inline def validate(value: Int): Option[Nothing] = None
  inline def apply(inline value: Int): FlowInfo = value

  /** Default flow info (zero). */
  val Default: FlowInfo = 0

  extension (fi: FlowInfo)
    /** Get the underlying 32-bit value. */
    inline def value: Int = fi

    /** Get the traffic class (upper 8 bits of the 28 significant bits). */
    transparent inline def trafficClass: Int = (fi >>> 20) & 0xff

    /** Get the flow label (lower 20 bits). */
    transparent inline def flowLabel: Int = fi & 0xfffff

end FlowInfo

/** IPv6 scope identifier for link-local addresses.
  *
  * Identifies the network interface for link-local destinations. Instances may be constructed via
  * [[ScopeId$ ScopeId]].
  */
opaque type ScopeId = Int

/** Provides factories and extension syntax for [[ScopeId]]. */
object ScopeId extends OpaqueType[ScopeId, Int], OpaqueType.Eq[ScopeId]:
  type Error = Nothing

  given Ordering[ScopeId] = Ordering.Int

  inline def wrap(value: Int): ScopeId = value
  inline def unwrap(sid: ScopeId): Int = sid
  protected inline def validate(value: Int): Option[Nothing] = None
  inline def apply(inline value: Int): ScopeId = value

  /** Default scope ID (zero - unspecified). */
  val Default: ScopeId = 0

  extension (sid: ScopeId)
    /** Get the underlying 32-bit value. */
    inline def value: Int = sid

    /** True if this is the default (unspecified) scope. */
    transparent inline def isDefault: Boolean = sid == 0

end ScopeId
