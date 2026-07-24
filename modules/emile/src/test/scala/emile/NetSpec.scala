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

// Net.interfaces: the loopback is present, internal, address-parsed, and has no hardware address.
final class NetSpec extends EmileSuite:

  test("interfaces enumerates the host's addresses, including an internal loopback") {
    Net.interfaces.absolve.map: interfaces =>
      assert(interfaces.nonEmpty, "no interface addresses enumerated")
      val loopback = interfaces.filter(_.internal)
      assert(loopback.nonEmpty, "no internal (loopback) interface found")
      assert(loopback.forall(_.mac.isEmpty), "loopback reported a hardware address")
      assert(
        loopback.exists(i => i.address.toString == "127.0.0.1" || i.address.toString == "::1"),
        "no loopback address parsed to 127.0.0.1 or ::1"
      )
  }
