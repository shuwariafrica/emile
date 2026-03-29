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

import scala.compiletime.error

import boilerplate.*
import boilerplate.nullable.*

/** A TCP or UDP port number in the range [0, 65535].
  *
  * Zero-cost opaque type wrapping `Int`. Instances may be constructed via [[Port$ Port]].
  *
  * @see
  *   [[https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml IANA Port Numbers]]
  */
opaque type Port = Int

/** Provides factories, constants, and extension syntax for [[Port]]. */
object Port extends OpaqueType[Port, Int], OpaqueType.Eq[Port]:
  type Error = AddressError

  given Ordering[Port] = Ordering.Int

  inline val MinValue = 0
  inline val MaxValue = 65535

  /** Wildcard port (0) - lets the OS choose an ephemeral port. */
  val Wildcard: Port = 0

  // Well-known ports
  inline def SSH: Port = 22
  inline def DNS: Port = 53
  inline def HTTP: Port = 80
  inline def HTTPS: Port = 443
  inline def MySQL: Port = 3306
  inline def PostgreSQL: Port = 5432
  inline def Redis: Port = 6379
  inline def SQLServer: Port = 1433

  inline def wrap(value: Int): Port = value
  inline def unwrap(port: Port): Int = port

  protected inline def validate(value: Int): Option[AddressError] =
    if value >= MinValue && value <= MaxValue then None
    else Some(AddressError.InvalidPort(value))

  inline def apply(inline value: Int): Port =
    inline if value < MinValue || value > MaxValue then error("Port must be in range 0-65535")
    else value

  /** Parse a Port from a string representation. */
  def from(value: String | Null): Either[AddressError, Port] =
    value.either(AddressError.InvalidPortString("null", "null input")).flatMap { v =>
      val trimmed = v.trim
      if trimmed.isEmpty then Left(AddressError.InvalidPortString(v, "empty input"))
      else
        scala.util.Try(trimmed.toInt).toOption match
          case Some(n) => from(n)
          case None    => Left(AddressError.InvalidPortString(v, "non-numeric input"))
    }

  extension (p: Port)
    /** Get the underlying integer value. */
    inline def value: Int = p

    /** Append decimal representation to an Appendable. */
    def writeTo[A <: Appendable](out: A): A =
      out.append(java.lang.Integer.toString(p))
      out

    /** String representation. */
    def show: String =
      val sb = new java.lang.StringBuilder
      writeTo(sb): Unit
      sb.toString
  end extension

end Port
