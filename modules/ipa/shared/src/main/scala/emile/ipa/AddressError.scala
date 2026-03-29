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

import scala.util.control.NoStackTrace

/** Errors that can occur when parsing or validating IP addresses and ports.
  *
  * This is the error type for the emile-ipa module. It is separate from `EmileError` (in
  * emile-core) as emile-ipa is a standalone cross-platform module with no dependencies.
  *
  * All errors extend `Throwable` with `NoStackTrace` to enable seamless integration with ecosystem
  * libraries whilst maintaining zero-cost error handling via `Either`.
  *
  * ==Converting to EmileError==
  *
  * When using emile-ipa types in emile-core (Native only), use the `toEmileError` extension method
  * to convert to the core error type.
  */
enum AddressError extends Throwable with NoStackTrace with Product with Serializable derives CanEqual:
  /** Port value is outside the valid range [0, 65535].
    *
    * @param value The invalid port value
    */
  case InvalidPort(value: Int)

  /** Port string is malformed or not numeric.
    *
    * @param input The original input string
    * @param detail Description of what was wrong
    */
  case InvalidPortString(input: String, detail: String)

  /** IPv4 address string is malformed.
    *
    * @param input The original input string
    * @param detail Description of what was wrong
    */
  case InvalidIpv4(input: String, detail: String)

  /** IPv6 address string is malformed.
    *
    * @param input The original input string
    * @param detail Description of what was wrong
    */
  case InvalidIpv6(input: String, detail: String)

  /** Socket address string is malformed.
    *
    * @param input The original input string
    * @param detail Description of what was wrong
    */
  case InvalidSocketAddress(input: String, detail: String)

  /** A user-friendly message for the error. */
  inline def message: String = this match
    case InvalidPort(v)             => s"Invalid port: $v (must be in range 0-65535)"
    case InvalidPortString(i, d)    => s"Invalid port '$i': $d (must be in range 0-65535)"
    case InvalidIpv4(i, d)          => s"Invalid IPv4 address '$i': $d"
    case InvalidIpv6(i, d)          => s"Invalid IPv6 address '$i': $d"
    case InvalidSocketAddress(i, d) => s"Invalid socket address '$i': $d"

  /** The underlying cause, for logging and debugging. */
  inline def cause: Option[Throwable] = None

  /** Override getMessage for Throwable integration. */
  override inline def getMessage: String = message

end AddressError

object AddressError
