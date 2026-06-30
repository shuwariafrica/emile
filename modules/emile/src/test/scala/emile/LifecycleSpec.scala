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
import fs2.Stream
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers the socket lifecycle under concurrency and after release: [[StreamServer.accepted]] keeps
  * each socket valid for its handler's `use` scope even when many run at once (the `server` preset
  * is used, so the finish-socket options are also applied on the accept path), and an operation on
  * a socket whose resource has released fails with [[EmileError.Io.AlreadyClosed]] rather than a
  * use-after-free.
  */
final class LifecycleSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("accepted sockets stay valid through concurrent slow handlers") {
    val payload: Chunk[Byte] = Chunk.array("concurrent-emile".getBytes("UTF-8"))
    Tcp
      .bind(anyLoopback, TcpOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(concurrentEcho(server, payload, 6)))
      .absolve
      .timeout(30.seconds)
  }

  test("an operation on a released socket fails with AlreadyClosed, not a use-after-free") {
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.liftF(
          for
            addr <- IO(server.address)
            leaked <- Tcp.connect(addr).widen[EmileError].use(socket => EffIO.succeed(socket)).absolve
            result <- leaked.read(4096).either
          yield assertEquals(result, Left(EmileError.Io.AlreadyClosed): Either[EmileError.Io, Option[Chunk[Byte]]])
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  private def concurrentEcho(server: TcpServer, payload: Chunk[Byte], n: Int): IO[Unit] =
    val addr = server.address
    val srvWork: IO[Unit] =
      server.accepted
        .parEvalMapUnordered(n)(
          _.use(socket =>
            socket
              .read(payload.size)
              .flatMap:
                // hold the socket across a slow step: early release would fail the echo write
                case Some(chunk) => EffIO.liftF(IO.sleep(150.millis)) *> socket.write(chunk)
                case None => EffIO.succeed(())
          )
        )
        .take(n.toLong)
        .compile
        .drain
        .absolve
    val client: IO[Unit] =
      Tcp
        .connect(addr)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF(
            for
              _ <- socket.write(payload).absolve
              echo <- socket.read(payload.size).absolve
              _ <- IO(assertEquals(echo.fold(List.empty[Byte])(_.toList), payload.toList))
            yield ()
          )
        )
        .absolve
    srvWork.background.use(_ => Stream.emits(0 until n).covary[IO].parEvalMapUnordered(n)(_ => client).compile.drain)
  end concurrentEcho

end LifecycleSpec
