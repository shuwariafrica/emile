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
import cats.effect.IO
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// Covers [[Socket.setKeepAlive]] - the keep-alive value guard, applied on a live connected socket.
final class TCPKeepAliveSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("keep-alive with valid windows is applied, and None disables it") {
    withConnected { socket =>
      for
        enabled <- socket.setKeepAlive(Some(TCPKeepAlive(1.second, 1.second, 1))).either
        _ <- IO(assert(enabled.isRight, s"enabling keep-alive failed: $enabled"))
        disabled <- socket.setKeepAlive(None).either
        _ <- IO(assert(disabled.isRight, s"disabling keep-alive failed: $disabled"))
      yield ()
    }
  }

  test("a sub-second idle or interval is rejected before it rounds to zero") {
    withConnected { socket =>
      for
        idle <- socket.setKeepAlive(Some(TCPKeepAlive(500.millis, 1.second, 5))).either
        _ <- IO(assertInvalid(idle))
        interval <- socket.setKeepAlive(Some(TCPKeepAlive(1.second, 900.millis, 5))).either
        _ <- IO(assertInvalid(interval))
      yield ()
    }
  }

  test("a non-positive probe count is rejected") {
    withConnected(socket => socket.setKeepAlive(Some(TCPKeepAlive(1.second, 1.second, 0))).either.map(assertInvalid))
  }

  private def assertInvalid(result: Either[EmileError.IO, Unit]): Unit =
    result match
      case Left(_: EmileError.IO.InvalidArgument) => ()
      case other => fail(s"expected an InvalidArgument rejection, got: $other")

  // Binds a server holding one accepted peer open, connects a client, and runs `body` on it.
  private def withConnected(body: TCPSocket => IO[Unit]): IO[Unit] =
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use { server =>
        val hold: IO[Unit] = server.accepted.evalMap(_.use(_ => EffIO.liftF(IO.never[Unit]))).compile.drain.absolve
        val client: IO[Unit] =
          TCP.connect(server.address).widen[EmileError].use(socket => EffIO.liftF(body(socket))).absolve
        EffIO.liftF(hold.background.surround(client))
      }
      .absolve
      .timeout(5.seconds)

end TCPKeepAliveSpec
