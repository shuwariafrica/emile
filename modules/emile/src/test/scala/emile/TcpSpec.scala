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

/** Covers [[Tcp.bind]] / [[Tcp.connect]] / [[StreamServer.accepted]] and the read / write socket
  * surface end-to-end over a loopback round-trip.
  */
final class TcpSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("bind on loopback yields a loopback address with a kernel-picked port") {
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.suspend:
          val addr = server.address
          assert(addr.host.isLoopback)
          assert(addr.port.value != 0)
      )
      .absolve
      .timeout(5.seconds)
  }

  test("server-client echo via read + write") {
    val payload: Chunk[Byte] = Chunk.array("hello, emile!".getBytes("UTF-8"))
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(echoRoundTrip(server, payload)))
      .absolve
      .timeout(5.seconds)
  }

  test("stress: 50 sequential connect+echo cycles exercise the GC under load") {
    val payload: Chunk[Byte] = Chunk.array("ping".getBytes("UTF-8"))
    val iterations = 50
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(stressEcho(server, payload, iterations)))
      .absolve
      .timeout(60.seconds)
  }

  test("reads + writes + endOfOutput stream the payload through a pipe echo") {
    val payload: Chunk[Byte] = Chunk.array("streaming-emile-payload".getBytes("UTF-8"))
    Tcp
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(streamingEcho(server, payload)))
      .absolve
      .timeout(5.seconds)
  }

  test("connect to a closed port fails on the Connect channel") {
    val closedAddress: IO[SocketAddress[IpAddress]] =
      Tcp.bind(anyLoopback).use(server => EffIO.suspend(server.address)).absolve
    closedAddress
      .flatMap(addr => Tcp.connect(addr).use(_ => EffIO.suspend(())).either)
      .map {
        case Left(_: EmileError.Connect) => ()
        case other => fail(s"expected a Connect error, got: $other")
      }
      .timeout(5.seconds)
  }

  private def echoRoundTrip(server: TcpServer, payload: Chunk[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      server.accepted
        .evalMap(
          _.use(socket =>
            socket
              .read(payload.size)
              .flatMap:
                case Some(chunk) => socket.write(chunk)
                case None => EffIO.succeed(())
          )
        )
        .head
        .compile
        .drain
        .absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
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

    srvWork.background.use(_ => cliWork)
  end echoRoundTrip

  private def streamingEcho(server: TcpServer, payload: Chunk[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      server.accepted
        .evalMap(_.use(socket => socket.reads.through(socket.writes).compile.drain.flatMap(_ => socket.endOfOutput)))
        .head
        .compile
        .drain
        .absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
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

    srvWork.background.use(_ => cliWork)
  end streamingEcho

  private def stressEcho(server: TcpServer, payload: Chunk[Byte], iterations: Int): IO[Unit] =
    val srvWork: IO[Unit] =
      server.accepted
        .evalMap(
          _.use(socket =>
            socket
              .read(payload.size)
              .flatMap:
                case Some(chunk) => socket.write(chunk)
                case None => EffIO.succeed(())
          )
        )
        .take(iterations.toLong)
        .compile
        .drain
        .absolve

    val oneCycle: IO[Unit] =
      Tcp
        .connect(server.address)
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

    srvWork.background.use(_ => oneCycle.replicateA_(iterations))
  end stressEcho

end TcpSpec
