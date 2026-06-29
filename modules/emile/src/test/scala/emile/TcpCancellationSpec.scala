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
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers the cancellation finaliser on [[Socket.read]]: a read whose `IO.async` is cancelled must
  * clear the libuv `uv_read_start` state so a subsequent read does not hit `UV_EALREADY`.
  */
final class TcpCancellationSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("a cancelled read leaves the socket readable on the next attempt") {
    val payload: Chunk[Byte] = Chunk.array("post-cancel".getBytes("UTF-8"))
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(cancelThenRead(server, payload)))
      .absolve
      .timeout(5.seconds)
  }

  private def cancelThenRead(server: TcpServer, payload: Chunk[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      server.accepted
        .evalMap(
          _.use(socket =>
            // Sleep past the client's first-read timeout, then deliver. The client will have
            // cancelled the first read by then; the second read is what receives this write.
            EffIO.liftF(IO.sleep(200.millis)) *> socket.write(payload)
          )
        )
        .head
        .compile
        .drain
        .absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address.asIpUnsafe)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF(
            for
              first <- socket.read(64).absolve.timeoutTo(50.millis, IO.pure(Option.empty[Chunk[Byte]]))
              _ <- IO(assertEquals(first, None))
              second <- socket.read(64).absolve
              _ <- IO(assertEquals(second.fold(List.empty[Byte])(_.toList), payload.toList))
            yield ()
          )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end cancelThenRead

end TcpCancellationSpec
