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

/** Covers [[Socket.proxy]]: a front client's bytes reach a backend and the backend's reply returns,
  * and a half-close on the front is propagated through to the backend so its echo terminates.
  */
final class ProxySpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("proxy bridges a front client to a backend echo, both directions, with half-close propagation") {
    val payload: Chunk[Byte] = Chunk.array("through-the-proxy".getBytes("UTF-8"))
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(backend =>
        TCP
          .bind(anyLoopback, TCPOptions.server)
          .widen[EmileError]
          .use(front => EffIO.liftF(runProxy(backend, front, payload)))
      )
      .absolve
      .timeout(15.seconds)
  }

  test("proxy tears down the surviving direction when the other fails, instead of hanging") {
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(halfOpenProxy(server)))
      .absolve
      .timeout(15.seconds)
  }

  // Accept two connections: reset the first (so that client's read fails with ConnectionReset) and hold
  // the second idle (its read would otherwise block forever). proxy must cancel the idle direction when
  // the other fails - without the sibling-cancel it hangs and the timeout on the caller fires.
  private def halfOpenProxy(server: TCPServer): IO[Unit] =
    val serverSide: IO[Unit] =
      server.accepted.zipWithIndex
        .evalMap((accept, idx) => accept.use(sock => if idx == 0L then sock.closeReset else EffIO.liftF(IO.never[Unit])))
        .compile
        .drain
        .absolve
    serverSide.background.use(_ =>
      (for
        a <- TCP.connect(server.address).widen[EmileError]
        b <- TCP.connect(server.address).widen[EmileError]
      yield (a, b)).use((a, b) => EffIO.liftF(Socket.proxy(a, b).either.void)).absolve
    )

  private def runProxy(backend: TCPServer, front: TCPServer, payload: Chunk[Byte]): IO[Unit] =
    // The backend echoes its input then half-closes; its echo half terminates only once the front's
    // half-close reaches it through the proxy.
    val backendEcho: IO[Unit] =
      backend.accepted
        .evalMap(_.use(socket => socket.reads.through(socket.writes).compile.drain.flatMap(_ => socket.endOfOutput)))
        .head
        .compile
        .drain
        .absolve
    // The proxy accepts one front connection, dials the backend, and splices them.
    val proxyWork: IO[Unit] =
      front.accepted
        .evalMap(_.use(clientSide => EffIO.liftF(bridge(backend, clientSide))))
        .head
        .compile
        .drain
        .absolve
    backendEcho.background.use(_ => proxyWork.background.use(_ => frontClient(front, payload)))
  end runProxy

  private def bridge(backend: TCPServer, clientSide: TCPSocket): IO[Unit] =
    TCP
      .connect(backend.address)
      .widen[EmileError]
      .use(backendSide => EffIO.liftF(Socket.proxy(clientSide, backendSide).absolve))
      .absolve

  private def frontClient(front: TCPServer, payload: Chunk[Byte]): IO[Unit] =
    TCP
      .connect(front.address)
      .widen[EmileError]
      .use(socket =>
        EffIO.liftF(
          for
            _ <- socket.write(payload).absolve
            _ <- socket.endOfOutput.absolve
            echo <- socket.reads.compile.to(Chunk).absolve
            _ <- IO(assertEquals(echo.toList, payload.toList))
          yield ()
        )
      )
      .absolve

end ProxySpec
