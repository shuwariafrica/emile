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
import cats.effect.unsafe.metrics.PollerMetrics
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Guards that a [[LibUVPoller]]'s I/O operations reach cats-effect's `PollerMetrics`, so emile I/O
  * is visible in `IORuntimeMetrics` rather than reading as all-zero.
  */
final class MetricsSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  // A libuv operation is counted on whichever worker owns its socket, so sum the projection across
  // every worker's poller.
  private def total(select: PollerMetrics => Long): IO[Long] =
    IO:
      EmileSuite.SharedRuntime.metrics.workStealingThreadPool.toList
        .flatMap(_.workerThreads)
        .map(worker => select(worker.poller))
        .sum

  test("a TCP round-trip records read, write, connect, and accept operations in IORuntimeMetrics") {
    val payload: Chunk[Byte] = Chunk.array("metrics-emile".getBytes("UTF-8"))
    for
      readBefore <- total(_.totalReadOperationsSucceededCount())
      writeBefore <- total(_.totalWriteOperationsSucceededCount())
      connectBefore <- total(_.totalConnectOperationsSucceededCount())
      acceptBefore <- total(_.totalAcceptOperationsSucceededCount())
      _ <- TCP.bind(anyLoopback).widen[EmileError].use(server => EffIO.liftF(echoRoundTrip(server, payload))).absolve
      readAfter <- total(_.totalReadOperationsSucceededCount())
      writeAfter <- total(_.totalWriteOperationsSucceededCount())
      connectAfter <- total(_.totalConnectOperationsSucceededCount())
      acceptAfter <- total(_.totalAcceptOperationsSucceededCount())
      _ <- IO:
             assert(readAfter > readBefore, s"read succeeded did not increase: $readBefore -> $readAfter")
             assert(writeAfter > writeBefore, s"write succeeded did not increase: $writeBefore -> $writeAfter")
             assert(connectAfter > connectBefore, s"connect succeeded did not increase: $connectBefore -> $connectAfter")
             assert(acceptAfter > acceptBefore, s"accept succeeded did not increase: $acceptBefore -> $acceptAfter")
    yield ()
    end for
  }

  private def echoRoundTrip(server: TCPServer, payload: Chunk[Byte]): IO[Unit] =
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
      TCP
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

    srvWork.background.use(_ => cliWork).timeout(10.seconds)
  end echoRoundTrip

end MetricsSpec
