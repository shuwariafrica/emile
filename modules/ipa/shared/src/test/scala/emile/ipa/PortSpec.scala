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

import munit.FunSuite

/** Tests for Port opaque type. */
class PortSpec extends FunSuite:
// scalafix:off

  private def expectRight[A](either: Either[AddressError, A]): A =
    either.fold(err => fail(err.message), identity)

  // ============================================================
  // from(Int) tests - runtime validation
  // ============================================================

  test("Port.from(Int) accepts minimum port (0)"):
    val result = Port.from(0)
    assert(result.isRight)
    assertEquals(result.map(_.value), Right(0))

  test("Port.from(Int) accepts maximum port (65535)"):
    val result = Port.from(65535)
    assert(result.isRight)
    assertEquals(result.map(_.value), Right(65535))

  test("Port.from accepts common ports"):
    val commonPorts = List(80, 443, 8080, 22, 3000, 5432, 27017)
    commonPorts.foreach { portNum =>
      val result = Port.from(portNum)
      assert(result.isRight, s"Port $portNum should be valid")
      assertEquals(result.map(_.value), Right(portNum))
    }

  test("Port.from(Int) rejects negative values"):
    val result = Port.from(-1)
    assert(result.isLeft)
    result.left.foreach { err =>
      assert(err.isInstanceOf[AddressError.InvalidPort])
      assertEquals(err.asInstanceOf[AddressError.InvalidPort].value, -1)
    }

  test("Port.from(Int) rejects large negative values"):
    val result = Port.from(-65536)
    assert(result.isLeft)

  test("Port.from(Int) rejects values above 65535"):
    val result = Port.from(65536)
    assert(result.isLeft)
    result.left.foreach { err =>
      assertEquals(err.asInstanceOf[AddressError.InvalidPort].value, 65536)
    }

  test("Port.from(Int) rejects large positive values"):
    val result = Port.from(70000)
    assert(result.isLeft)

  test("Port.from(Int) rejects Int.MaxValue"):
    val result = Port.from(Int.MaxValue)
    assert(result.isLeft)

  test("Port.from(Int) rejects Int.MinValue"):
    val result = Port.from(Int.MinValue)
    assert(result.isLeft)

  // ============================================================
  // from(String) tests - string parsing
  // ============================================================

  test("Port.from(String) parses valid port"):
    val result = Port.from("8080")
    assertEquals(expectRight(result).value, 8080)

  test("Port.from(String) parses zero"):
    val result = Port.from("0")
    assertEquals(expectRight(result).value, 0)

  test("Port.from(String) parses maximum port"):
    val result = Port.from("65535")
    assertEquals(expectRight(result).value, 65535)

  test("Port.from(String) returns error for empty string"):
    val result = Port.from("")
    assert(result.isLeft)

  test("Port.from(String) returns error for non-numeric"):
    val result = Port.from("http")
    assert(result.isLeft)

  test("Port.from(String) returns error for negative string"):
    val result = Port.from("-1")
    assert(result.isLeft)

  test("Port.from(String) returns error for out-of-range"):
    val result = Port.from("65536")
    assert(result.isLeft)

  test("Port.from(String) returns error for whitespace"):
    val result = Port.from("  ")
    assert(result.isLeft)

  test("Port.from(String) trims leading whitespace then validates"):
    val result = Port.from(" 8080")
    assertEquals(expectRight(result).value, 8080)

  test("Port.from(String) returns error for decimal notation"):
    val result = Port.from("80.0")
    assert(result.isLeft)

  test("Port.from(String) returns error for hex notation"):
    val result = Port.from("0x50")
    assert(result.isLeft)

  // ============================================================
  // wrap tests (unchecked construction via OpaqueType)
  // ============================================================

  test("Port.wrap creates port for valid value"):
    val port = Port.wrap(443)
    assertEquals(port.value, 443)

  test("Port.wrap creates port for boundary values"):
    assertEquals(Port.wrap(0).value, 0)
    assertEquals(Port.wrap(65535).value, 65535)

  // ============================================================
  // value extraction tests
  // ============================================================

  test("Port.value returns the underlying integer"):
    val port = expectRight(Port.from(5432))
    assertEquals(port.value, 5432)

  test("Port.value roundtrips correctly"):
    val values = List(0, 1, 80, 443, 8080, 32767, 65534, 65535)
    values.foreach { v =>
      val port = expectRight(Port.from(v))
      assertEquals(port.value, v)
    }

  // ============================================================
  // literals interpolation tests
  // ============================================================

  test("port interpolator supports mixed literal and values"):
    import emile.ipa.literals.*

    val suffix = 80
    val port = port"80${suffix}"
    assertEquals(port.value, 8080)

  test("port interpolator rejects invalid literal fragments"):
    val errors = compileErrors(
      """import emile.ipa.literals.*

val bad = port"80a${1}"
"""
    )
    assert(errors.contains("Invalid port literal fragment"))

  // ============================================================
  // show tests
  // ============================================================

  test("Port.show returns string representation"):
    val port = expectRight(Port.from(8080))
    assertEquals(port.show, "8080")

  test("Port.show for zero"):
    val port = expectRight(Port.from(0))
    assertEquals(port.show, "0")

  test("Port.show for max"):
    val port = expectRight(Port.from(65535))
    assertEquals(port.show, "65535")

  // ============================================================
  // writeTo tests
  // ============================================================

  test("Port.writeTo appends to Appendable"):
    val sb = new java.lang.StringBuilder("port=")
    val port = expectRight(Port.from(1234))
    val result = port.writeTo(sb)
    assertEquals(sb.toString, "port=1234")
    assertEquals(result, sb)

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
    val port0 = expectRight(Port.from(0))
    assertEquals(Port.Wildcard, port0)

  // ============================================================
  // Well-known port constants tests
  // ============================================================

  test("Port.SSH is 22"):
    assertEquals(Port.SSH.value, 22)

  test("Port.DNS is 53"):
    assertEquals(Port.DNS.value, 53)

  test("Port.HTTP is 80"):
    assertEquals(Port.HTTP.value, 80)

  test("Port.HTTPS is 443"):
    assertEquals(Port.HTTPS.value, 443)

  test("Port.MySQL is 3306"):
    assertEquals(Port.MySQL.value, 3306)

  test("Port.PostgreSQL is 5432"):
    assertEquals(Port.PostgreSQL.value, 5432)

  test("Port.Redis is 6379"):
    assertEquals(Port.Redis.value, 6379)

  test("Port.SQLServer.SQLServer is 1433"):
    assertEquals(Port.SQLServer.value, 1433)

  // ============================================================
  // Ordering tests
  // ============================================================

  test("Port.Ordering compares ports correctly"):
    val p1 = expectRight(Port.from(80))
    val p2 = expectRight(Port.from(443))
    val p3 = expectRight(Port.from(80))

    assert(Ordering[Port].lt(p1, p2), "80 < 443")
    assert(Ordering[Port].gt(p2, p1), "443 > 80")
    assert(Ordering[Port].equiv(p1, p3), "80 == 80")

  test("Port.Ordering sorts list correctly"):
    val ports = List(443, 80, 8080, 22, 3000).map(n => expectRight(Port.from(n)))
    val sorted = ports.sorted
    assertEquals(sorted.map(_.value), List(22, 80, 443, 3000, 8080))

  test("Port.Ordering handles boundary values"):
    val min = expectRight(Port.from(0))
    val max = expectRight(Port.from(65535))
    assert(Ordering[Port].lt(min, max))
    assert(Ordering[Port].gt(max, min))

  // ============================================================
  // Equality tests
  // ============================================================

  test("Port equality for same value"):
    val p1 = expectRight(Port.from(8080))
    val p2 = expectRight(Port.from(8080))
    assertEquals(p1, p2)
    assert(p1 == p2)

  test("Port inequality for different values"):
    val p1 = expectRight(Port.from(80))
    val p2 = expectRight(Port.from(443))
    assertNotEquals(p1, p2)

end PortSpec
