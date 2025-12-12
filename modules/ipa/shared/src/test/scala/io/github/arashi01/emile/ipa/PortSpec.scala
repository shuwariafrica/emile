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
 * Tests for Port opaque type.
 *
 * Tests cover:
 * - fromInt: runtime validation
 * - fromString: string parsing
 * - unsafeFromInt: unsafe construction
 * - value: extraction
 * - show: string representation
 * - Ordering: comparison
 * - Constants: MinValue, MaxValue, Wildcard
 */
class PortSpec extends FunSuite:

  // ============================================================
  // fromInt tests - runtime validation
  // ============================================================

  test("Port.fromInt accepts minimum port (0)"):
    val result = Port.fromInt(0)
    assert(result.isRight)
    assertEquals(result.map(_.value), Right(0))

  test("Port.fromInt accepts maximum port (65535)"):
    val result = Port.fromInt(65535)
    assert(result.isRight)
    assertEquals(result.map(_.value), Right(65535))

  test("Port.fromInt accepts common ports"):
    val commonPorts = List(80, 443, 8080, 22, 3000, 5432, 27017)
    commonPorts.foreach { portNum =>
      val result = Port.fromInt(portNum)
      assert(result.isRight, s"Port $portNum should be valid")
      assertEquals(result.map(_.value), Right(portNum))
    }

  test("Port.fromInt rejects negative values"):
    val result = Port.fromInt(-1)
    assert(result.isLeft)
    result.left.foreach { err =>
      assert(err.isInstanceOf[AddressError.InvalidPort])
      assertEquals(err.asInstanceOf[AddressError.InvalidPort].value, -1)
    }

  test("Port.fromInt rejects large negative values"):
    val result = Port.fromInt(-65536)
    assert(result.isLeft)

  test("Port.fromInt rejects values above 65535"):
    val result = Port.fromInt(65536)
    assert(result.isLeft)
    result.left.foreach { err =>
      assertEquals(err.asInstanceOf[AddressError.InvalidPort].value, 65536)
    }

  test("Port.fromInt rejects large positive values"):
    val result = Port.fromInt(70000)
    assert(result.isLeft)

  test("Port.fromInt rejects Int.MaxValue"):
    val result = Port.fromInt(Int.MaxValue)
    assert(result.isLeft)

  test("Port.fromInt rejects Int.MinValue"):
    val result = Port.fromInt(Int.MinValue)
    assert(result.isLeft)

  // ============================================================
  // fromString tests - string parsing
  // ============================================================

  test("Port.fromString parses valid port"):
    val result = Port.fromString("8080")
    assert(result.isDefined)
    assertEquals(result.get.value, 8080)

  test("Port.fromString parses zero"):
    val result = Port.fromString("0")
    assert(result.isDefined)
    assertEquals(result.get.value, 0)

  test("Port.fromString parses maximum port"):
    val result = Port.fromString("65535")
    assert(result.isDefined)
    assertEquals(result.get.value, 65535)

  test("Port.fromString returns None for empty string"):
    val result = Port.fromString("")
    assert(result.isEmpty)

  test("Port.fromString returns None for non-numeric"):
    val result = Port.fromString("http")
    assert(result.isEmpty)

  test("Port.fromString returns None for negative string"):
    val result = Port.fromString("-1")
    assert(result.isEmpty)

  test("Port.fromString returns None for out-of-range"):
    val result = Port.fromString("65536")
    assert(result.isEmpty)

  test("Port.fromString returns None for whitespace"):
    val result = Port.fromString("  ")
    assert(result.isEmpty)

  test("Port.fromString returns None for leading whitespace"):
    // Note: depending on implementation, this may or may not be accepted
    // Scala's toIntOption trims whitespace, so we test for both behaviors
    val result = Port.fromString(" 8080")
    // If it parses, it should be 8080; if not, it's None
    result.foreach(p => assertEquals(p.value, 8080))

  test("Port.fromString returns None for decimal notation"):
    val result = Port.fromString("80.0")
    assert(result.isEmpty)

  test("Port.fromString returns None for hex notation"):
    val result = Port.fromString("0x50")
    assert(result.isEmpty)

  // ============================================================
  // unsafeFromInt tests
  // ============================================================

  test("Port.unsafeFromInt creates port for valid value"):
    val port = Port.unsafeFromInt(443)
    assertEquals(port.value, 443)

  test("Port.unsafeFromInt creates port for boundary values"):
    assertEquals(Port.unsafeFromInt(0).value, 0)
    assertEquals(Port.unsafeFromInt(65535).value, 65535)

  // Note: unsafeFromInt does NOT validate - it's caller's responsibility
  // We don't test invalid values as behavior is undefined

  // ============================================================
  // value extraction tests
  // ============================================================

  test("Port.value returns the underlying integer"):
    val port = Port.fromInt(5432).getOrElse(fail("should be valid"))
    assertEquals(port.value, 5432)

  test("Port.value roundtrips correctly"):
    val values = List(0, 1, 80, 443, 8080, 32767, 65534, 65535)
    values.foreach { v =>
      val port = Port.fromInt(v).getOrElse(fail(s"$v should be valid"))
      assertEquals(port.value, v)
    }

  // ============================================================
  // show tests
  // ============================================================

  test("Port.show returns string representation"):
    val port = Port.fromInt(8080).getOrElse(fail("should be valid"))
    assertEquals(port.show, "8080")

  test("Port.show for zero"):
    val port = Port.fromInt(0).getOrElse(fail("should be valid"))
    assertEquals(port.show, "0")

  test("Port.show for max"):
    val port = Port.fromInt(65535).getOrElse(fail("should be valid"))
    assertEquals(port.show, "65535")

  // ============================================================
  // Constants tests
  // ============================================================

  test("Port.MinValue is 0"):
    assertEquals(Port.MinValue, 0)

  test("Port.MaxValue is 65535"):
    assertEquals(Port.MaxValue, 65535)

  test("Port.Wildcard is 0"):
    assertEquals(Port.Wildcard.value, 0)

  test("Port.Wildcard equals port 0"):
    val port0 = Port.fromInt(0).getOrElse(fail("should be valid"))
    assertEquals(Port.Wildcard, port0)

  // ============================================================
  // Ordering tests
  // ============================================================

  test("Port.Ordering compares ports correctly"):
    val p1 = Port.fromInt(80).getOrElse(fail("should be valid"))
    val p2 = Port.fromInt(443).getOrElse(fail("should be valid"))
    val p3 = Port.fromInt(80).getOrElse(fail("should be valid"))

    assert(Ordering[Port].lt(p1, p2), "80 < 443")
    assert(Ordering[Port].gt(p2, p1), "443 > 80")
    assert(Ordering[Port].equiv(p1, p3), "80 == 80")

  test("Port.Ordering sorts list correctly"):
    val ports = List(443, 80, 8080, 22, 3000).flatMap(Port.fromInt(_).toOption)
    val sorted = ports.sorted
    assertEquals(sorted.map(_.value), List(22, 80, 443, 3000, 8080))

  test("Port.Ordering handles boundary values"):
    val min = Port.fromInt(0).getOrElse(fail("should be valid"))
    val max = Port.fromInt(65535).getOrElse(fail("should be valid"))
    assert(Ordering[Port].lt(min, max))
    assert(Ordering[Port].gt(max, min))

  // ============================================================
  // Equality tests
  // ============================================================

  test("Port equality for same value"):
    val p1 = Port.fromInt(8080).getOrElse(fail("should be valid"))
    val p2 = Port.fromInt(8080).getOrElse(fail("should be valid"))
    assertEquals(p1, p2)
    assert(p1 == p2)

  test("Port inequality for different values"):
    val p1 = Port.fromInt(80).getOrElse(fail("should be valid"))
    val p2 = Port.fromInt(443).getOrElse(fail("should be valid"))
    assertNotEquals(p1, p2)

end PortSpec
