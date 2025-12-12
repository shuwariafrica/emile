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
 * Errors that can occur when parsing or validating IP addresses and ports.
 *
 * This is the error type for the emile-ipa module. It is separate from
 * `EmileError` (in emile-core) as emile-ipa is a standalone cross-platform
 * module with no dependencies.
 *
 * == Converting to EmileError ==
 *
 * When using emile-ipa types in emile-core (Native only), use the
 * `toEmileError` extension method to convert to the core error type.
 */
enum AddressError:
  /**
   * Port value is outside the valid range [0, 65535].
   *
   * @param value
   *   The invalid port value
   */
  case InvalidPort(value: Int)

  /**
   * IPv4 address string is malformed.
   *
   * @param input
   *   The original input string
   * @param detail
   *   Description of what was wrong
   */
  case InvalidIpv4(input: String, detail: String)

  /**
   * IPv6 address string is malformed.
   *
   * @param input
   *   The original input string
   * @param detail
   *   Description of what was wrong
   */
  case InvalidIpv6(input: String, detail: String)

  /**
   * Socket address string is malformed.
   *
   * @param input
   *   The original input string
   * @param detail
   *   Description of what was wrong
   */
  case InvalidSocketAddress(input: String, detail: String)

  /**
   * Human-readable error message.
   */
  def message: String = this match
    case InvalidPort(v)             => s"Invalid port: $v (must be in range 0-65535)"
    case InvalidIpv4(i, d)          => s"Invalid IPv4 address '$i': $d"
    case InvalidIpv6(i, d)          => s"Invalid IPv6 address '$i': $d"
    case InvalidSocketAddress(i, d) => s"Invalid socket address '$i': $d"

end AddressError

object AddressError:
  given CanEqual[AddressError, AddressError] = CanEqual.derived
