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

import java.net.StandardSocketOptions
import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import cats.effect.IO
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

import emile.Fs2Interop.*

/** Covers [[Fs2Interop.asFs2]] and [[Fs2Interop.acceptFs2]]: an emile [[TcpSocket]] / [[TcpServer]]
  * adapts to `fs2.io.net.Socket[IO]` / `Stream[IO, Socket[IO]]` and round-trips a payload through
  * fs2's typed-`IO` API on both sides.
  */
final class Fs2InteropSpec extends Fs2EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("asFs2 + acceptFs2 round-trip a payload through fs2's Socket[IO] API on both sides") {
    val payload: Chunk[Byte] = Chunk.array("emile-fs2-interop".getBytes("UTF-8"))
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(fs2RoundTrip(server, payload)))
      .absolve
      .timeout(5.seconds)
  }

  test("setOption(SO_KEEPALIVE, true) succeeds via the emile keep-alive setter") {
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.liftF(
          Tcp
            .connect(server.address)
            .widen[EmileError]
            .use(socket => EffIO.liftF(socket.asFs2.setOption(StandardSocketOptions.SO_KEEPALIVE, java.lang.Boolean.TRUE)))
            .absolve
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("setOption with an unsupported key raises IllegalArgumentException on the Throwable channel") {
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.liftF(
          Tcp
            .connect(server.address)
            .widen[EmileError]
            .use(socket =>
              EffIO.liftF(
                socket.asFs2
                  .setOption(StandardSocketOptions.SO_RCVBUF, java.lang.Integer.valueOf(8192))
                  .attempt
                  .map {
                    case Left(_: IllegalArgumentException) => ()
                    case other => fail(s"expected IllegalArgumentException, got: $other")
                  }
              )
            )
            .absolve
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  private def fs2RoundTrip(server: TcpServer, payload: Chunk[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      server.acceptFs2
        .evalMap(fs2Sock =>
          fs2Sock.read(payload.size).flatMap {
            case Some(chunk) => fs2Sock.write(chunk)
            case None => IO.unit
          }
        )
        .head
        .compile
        .drain

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF {
            val fs2Sock = socket.asFs2
            for
              _ <- fs2Sock.write(payload)
              echo <- fs2Sock.read(payload.size)
              _ <- IO(assertEquals(echo.fold(List.empty[Byte])(_.toList), payload.toList))
            yield ()
          }
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end fs2RoundTrip

end Fs2InteropSpec
