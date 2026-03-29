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

/** JavaScript platform extensions for IP address and socket address types.
  *
  * Currently a placeholder. JavaScript does not have direct socket access, but this module could be
  * extended to support:
  *
  *   - WebSocket URL generation
  *   - URL parsing and formatting
  *   - Integration with Node.js net module (via Scala.js facade)
  */

extension (addr: SocketAddress)
  /** Format the address as a URL host string.
    *
    * IPv6 addresses are wrapped in brackets as required by URL syntax.
    *
    * @return URL-formatted host:port string
    */
  def toUrlHost: String = addr match
    case SocketAddress.V4(ipv4, port)       => s"${ipv4.show}:${port.value}"
    case SocketAddress.V6(ipv6, port, _, _) => s"[${ipv6.show}]:${port.value}"
