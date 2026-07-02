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
import cats.effect.Ref
import fs2.Chunk
import fs2.Stream
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers [[StreamServer.serve]]: it handles many connections concurrently with a bounded pool, and
  * isolates a failing handler through `onError` without stopping the server or its siblings.
  */
final class ServeSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  private val payload: Chunk[Byte] = Chunk.array("serve-emile".getBytes("UTF-8"))

  test("serve echoes across many concurrent connections") {
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(serveEcho(server, 12)))
      .absolve
      .timeout(20.seconds)
  }

  test("serve isolates a failing handler and keeps serving siblings") {
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(serveIsolation(server, 6)))
      .absolve
      .timeout(20.seconds)
  }

  private def serveEcho(server: TCPServer, n: Int): IO[Unit] =
    val handler = (socket: TCPSocket) =>
      socket
        .read(payload.size)
        .flatMap:
          case Some(chunk) => socket.write(chunk)
          case None => EffIO.succeed(())
    for
      shutdown <- IO.deferred[Unit]
      serveFib <- server.serve(8, shutdown.get)(_ => IO.unit)(handler).absolve.start
      _ <- Stream.emits(0 until n).covary[IO].parEvalMapUnordered(n)(_ => echoClient(server)).compile.drain
      _ <- shutdown.complete(())
      _ <- serveFib.join.timeout(15.seconds)
    yield ()

  private def echoClient(server: TCPServer): IO[Unit] =
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

  private def serveIsolation(server: TCPServer, n: Int): IO[Unit] =
    // Every handler fails; a resilient serve reports each through onError and keeps accepting, so all n
    // failures are observed and serve still shuts down cleanly.
    val handler = (_: TCPSocket) => EffIO.fail[EmileError.IO](EmileError.IO.BrokenPipe)
    for
      shutdown <- IO.deferred[Unit]
      errors <- Ref[IO].of(0)
      serveFib <- server.serve(4, shutdown.get)(_ => errors.update(_ + 1))(handler).absolve.start
      _ <- Stream.emits(0 until n).covary[IO].parEvalMapUnordered(n)(_ => connectClose(server)).compile.drain
      _ <- waitUntil(errors.get.map(_ >= n))
      _ <- shutdown.complete(())
      _ <- serveFib.join.timeout(15.seconds)
      count <- errors.get
      _ <- IO(assert(count >= n, s"expected at least $n isolated handler errors, got $count"))
    yield ()

  private def connectClose(server: TCPServer): IO[Unit] =
    TCP.connect(server.address).widen[EmileError].use(_ => EffIO.succeed(())).absolve

  private def waitUntil(cond: IO[Boolean]): IO[Unit] =
    cond.flatMap(done => if done then IO.unit else IO.sleep(20.millis) *> waitUntil(cond))

end ServeSpec
