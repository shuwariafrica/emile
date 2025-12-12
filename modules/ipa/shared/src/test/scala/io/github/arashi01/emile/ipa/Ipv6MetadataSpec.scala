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

import munit.FunSuite

/**
 * Tests for FlowInfo and ScopeId opaque types.
 *
 * Tests cover:
 * - FlowInfo: apply, value, trafficClass, flowLabel, Default
 * - ScopeId: apply, value, isDefault, Default
 */
class Ipv6MetadataSpec extends FunSuite:

  // ============================================================
  // FlowInfo tests
  // ============================================================

  test("FlowInfo.Default is 0"):
    assertEquals(FlowInfo.Default.value, 0)

  test("FlowInfo.apply creates from raw value"):
    val fi = FlowInfo(0x12345678)
    assertEquals(fi.value, 0x12345678)

  test("FlowInfo.value returns underlying integer"):
    val fi = FlowInfo(42)
    assertEquals(fi.value, 42)

  test("FlowInfo.trafficClass extracts upper 8 bits of 28 significant bits"):
    // Traffic class is in bits 20-27 (upper 8 bits of the 28-bit flow info)
    // Format: TTTTTTTT_LLLLLLLL_LLLLLLLL_LLLL (T=traffic class, L=flow label)
    val fi = FlowInfo(0x0AB00000) // Traffic class = 0xAB, flow label = 0
    assertEquals(fi.trafficClass, 0xAB)

  test("FlowInfo.trafficClass extracts correctly with flow label"):
    val fi = FlowInfo(0x0FF12345) // Traffic class = 0xFF, flow label = 0x12345
    assertEquals(fi.trafficClass, 0xFF)

  test("FlowInfo.flowLabel extracts lower 20 bits"):
    val fi = FlowInfo(0x000FFFFF) // Flow label = max 20-bit value
    assertEquals(fi.flowLabel, 0xFFFFF)

  test("FlowInfo.flowLabel extracts correctly with traffic class"):
    val fi = FlowInfo(0x0AB12345) // Traffic class = 0xAB, flow label = 0x12345
    assertEquals(fi.flowLabel, 0x12345)

  test("FlowInfo.flowLabel for zero"):
    val fi = FlowInfo(0xFFF00000) // Any traffic class, flow label = 0
    assertEquals(fi.flowLabel, 0)

  test("FlowInfo equality"):
    val fi1 = FlowInfo(0x12345678)
    val fi2 = FlowInfo(0x12345678)
    assertEquals(fi1, fi2)

  test("FlowInfo inequality"):
    val fi1 = FlowInfo(0x12345678)
    val fi2 = FlowInfo(0x87654321)
    assertNotEquals(fi1, fi2)

  test("FlowInfo Ordering"):
    val fi1 = FlowInfo(100)
    val fi2 = FlowInfo(200)
    assert(Ordering[FlowInfo].lt(fi1, fi2))

  // ============================================================
  // ScopeId tests
  // ============================================================

  test("ScopeId.Default is 0"):
    assertEquals(ScopeId.Default.value, 0)

  test("ScopeId.apply creates from raw value"):
    val sid = ScopeId(5)
    assertEquals(sid.value, 5)

  test("ScopeId.value returns underlying integer"):
    val sid = ScopeId(123)
    assertEquals(sid.value, 123)

  test("ScopeId.isDefault is true for 0"):
    assert(ScopeId.Default.isDefault)
    assert(ScopeId(0).isDefault)

  test("ScopeId.isDefault is false for non-zero"):
    assert(!ScopeId(1).isDefault)
    assert(!ScopeId(255).isDefault)

  test("ScopeId equality"):
    val sid1 = ScopeId(42)
    val sid2 = ScopeId(42)
    assertEquals(sid1, sid2)

  test("ScopeId inequality"):
    val sid1 = ScopeId(1)
    val sid2 = ScopeId(2)
    assertNotEquals(sid1, sid2)

  test("ScopeId Ordering"):
    val sid1 = ScopeId(1)
    val sid2 = ScopeId(10)
    assert(Ordering[ScopeId].lt(sid1, sid2))

  test("ScopeId can represent interface indices"):
    // Common use case: scope ID is the interface index for link-local addresses
    val eth0Index = ScopeId(2) // Typical Linux eth0 index
    assertEquals(eth0Index.value, 2)
    assert(!eth0Index.isDefault)

end Ipv6MetadataSpec
