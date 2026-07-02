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

/** Covers [[Socket.closeReset]] - the abortive RST close, from both the closing side (the socket is
  * closed afterwards) and the peer's side (it observes a reset, not a clean end of stream).
  */
final class TCPCloseResetSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("closeReset closes the socket; a subsequent operation is AlreadyClosed") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(resetThenReadClosed(server)))
      .absolve
      .timeout(5.seconds)
  }

  test("closeReset resets the connection; the peer's read stream ends with ConnectionReset") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(peerSeesReset(server)))
      .absolve
      .timeout(5.seconds)
  }

  test("closeReset terminates a concurrent in-flight read with AlreadyClosed rather than hanging it") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(resetTerminatesRead(server)))
      .absolve
      .timeout(5.seconds)
  }

  private def resetThenReadClosed(server: TCPServer): IO[Unit] =
    TCP
      .connect(server.address)
      .widen[EmileError]
      .use(socket =>
        EffIO.liftF(
          for
            _ <- socket.closeReset.absolve
            after <- socket.read(10).either
            _ <- IO(after match
                   case Left(EmileError.IO.AlreadyClosed) => ()
                   case other => fail(s"expected AlreadyClosed after closeReset, got: $other"))
          yield ()
        )
      )
      .absolve

  // The client resets only once the server has accepted and is about to read, so the RST is
  // deterministically observed on the peer's read rather than racing the accept.
  private def peerSeesReset(server: TCPServer): IO[Unit] =
    IO.deferred[Unit].flatMap { accepted =>
      val peer: IO[Either[EmileError.IO, Unit]] =
        server.accepted
          .evalMap(_.use(socket => EffIO.liftF(accepted.complete(()).void).flatMap(_ => socket.reads.compile.drain)))
          .head
          .compile
          .drain
          .either
      val client: IO[Unit] =
        TCP
          .connect(server.address)
          .widen[EmileError]
          .use(socket => EffIO.liftF(accepted.get).flatMap(_ => socket.closeReset))
          .absolve
      peer
        .both(client)
        .map((peerResult, _) =>
          peerResult match
            case Left(EmileError.IO.ConnectionReset) => ()
            case other => fail(s"expected the peer to observe ConnectionReset, got: $other")
        )
    }

  // The peer sends nothing, so the client's read stays armed and blocked; closeReset on the same
  // socket must fire the read's continuation (AlreadyClosed) rather than stop the read and leave it
  // waiting forever. joinWithNever would never return if the read hung, so the outer timeout fails it.
  private def resetTerminatesRead(server: TCPServer): IO[Unit] =
    IO.deferred[Unit].flatMap { accepted =>
      val hold: IO[Unit] =
        server.accepted.evalMap(_.use(_ => EffIO.liftF(accepted.complete(()).void *> IO.never[Unit]))).head.compile.drain.absolve
      hold.background.use(_ =>
        TCP
          .connect(server.address)
          .widen[EmileError]
          .use(socket =>
            EffIO.liftF(
              for
                _ <- accepted.get
                blocked <- socket.read(4096).either.start
                _ <- IO.sleep(150.millis)
                _ <- socket.closeReset.absolve
                result <- blocked.joinWithNever.timeout(3.seconds)
                _ <- IO(
                       assertEquals(result, Left(EmileError.IO.AlreadyClosed): Either[EmileError.IO, Option[fs2.Chunk[Byte]]])
                     )
              yield ()
            )
          )
          .absolve
      )
    }

end TCPCloseResetSpec
