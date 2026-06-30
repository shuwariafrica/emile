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

import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers [[TcpOptions.reusePort]] (`SO_REUSEPORT`): two binders share the same address. */
final class TcpReusePortSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("SO_REUSEPORT permits two binders on the same port") {
    Tcp
      .bind(anyLoopback, TcpOptions.server)
      .widen[EmileError]
      .use(first =>
        Tcp
          .bind(first.address, TcpOptions.server)
          .widen[EmileError]
          .use(second =>
            EffIO.suspend:
              assertEquals(second.address.port, first.address.port)
          )
      )
      .absolve
      .timeout(5.seconds)
  }

end TcpReusePortSpec
