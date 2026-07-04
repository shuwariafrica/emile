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

/** Covers the socket lifecycle under concurrency and after release: concurrent handlers keep their
  * sockets valid, use after release is a typed [[EmileError.IO.AlreadyClosed]], and a second
  * concurrent read fails with [[EmileError.IO.ConflictingOperation]].
  */
final class LifecycleSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("accepted sockets stay valid through concurrent slow handlers") {
    val payload: Chunk[Byte] = Chunk.array("concurrent-emile".getBytes("UTF-8"))
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(concurrentEcho(server, payload, 6)))
      .absolve
      .timeout(30.seconds)
  }

  test("an operation on a released socket fails with AlreadyClosed, not a use-after-free") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.liftF(
          for
            addr <- IO(server.address)
            leaked <- TCP.connect(addr).widen[EmileError].use(socket => EffIO.succeed(socket)).absolve
            result <- leaked.read(4096).either
          yield assertEquals(result, Left(EmileError.IO.AlreadyClosed): Either[EmileError.IO, Option[Chunk[Byte]]])
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("a second concurrent read fails fast with ConflictingOperation, not buffer corruption") {
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(conflictingRead(server)))
      .absolve
      .timeout(10.seconds)
  }

  test("an accepted socket actually carries the server preset's TCP_NODELAY") {
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(acceptedNoDelay(server)))
      .absolve
      .timeout(10.seconds)
  }

  // Reads the accepted socket's live TCP_NODELAY through the private getter, so the assertion is on the
  // option value the kernel holds, not merely that the finish-socket step ran (the earlier T1.2 bound).
  private def acceptedNoDelay(server: TCPServer): IO[Unit] =
    val accepted = server.accepted.head.evalMap(_.use(sock => Socket.readNoDelay(sock))).compile.lastOrError.absolve
    val client = TCP.connect(server.address).widen[EmileError].use(_ => EffIO.liftF(IO.sleep(300.millis))).absolve
    accepted.both(client).map((noDelay, _) => assert(noDelay, "accepted socket should have TCP_NODELAY set by the server preset"))

  private def concurrentEcho(server: TCPServer, payload: Chunk[Byte], n: Int): IO[Unit] =
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
      TCP
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

  // The server holds every accepted socket open without sending, so the client's first read blocks and
  // stays armed; a concurrent second read must then fail fast rather than race the shared read buffer.
  private def conflictingRead(server: TCPServer): IO[Unit] =
    val hold = server.accepted.parEvalMapUnordered(4)(_.use(_ => EffIO.liftF(IO.never[Unit]))).compile.drain.absolve
    hold.background.use(_ =>
      TCP
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF(
            for
              blocked <- socket.read(4096).absolve.start
              _ <- IO.sleep(150.millis)
              result <- socket.read(4096).either
              _ <- blocked.cancel
              _ <- IO(result match
                     case Left(EmileError.IO.ConflictingOperation) => ()
                     case other => fail(s"expected ConflictingOperation, got: $other"))
            yield ()
          )
        )
        .absolve
    )
  end conflictingRead

end LifecycleSpec
