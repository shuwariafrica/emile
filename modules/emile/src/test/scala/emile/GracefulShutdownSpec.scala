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
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers [[StreamServer.serve]]'s graceful-shutdown contract: once shutdown is requested the
  * server stops accepting, and an in-flight handler runs to completion (it drains, it is not
  * cancelled) before `serve` returns.
  */
final class GracefulShutdownSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("shutdown drains an in-flight handler to completion rather than cancelling it") {
    TCP
      .bind(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use(server => EffIO.liftF(drainInFlight(server)))
      .absolve
      .timeout(20.seconds)
  }

  private def drainInFlight(server: TCPServer): IO[Unit] =
    for
      started <- IO.deferred[Unit]
      completed <- Ref[IO].of(false)
      shutdown <- IO.deferred[Unit]
      // The handler signals it is in flight, does slow work, then records completion - which only runs
      // if the drain lets it finish rather than cancelling it mid-sleep.
      handler = (_: TCPSocket) => EffIO.liftF(started.complete(()).attempt.void *> IO.sleep(600.millis) *> completed.set(true))
      serveFib <- server.serve(4, shutdown.get)(_ => IO.unit)(handler).absolve.start
      // A client connects and holds the socket open so the handler stays in flight until shutdown.
      clientFib <- clientHold(server).start
      _ <- started.get
      _ <- shutdown.complete(())
      _ <- serveFib.join.timeout(15.seconds)
      done <- completed.get
      _ <- clientFib.cancel
      _ <- IO(assert(done, "the in-flight handler should have drained to completion, not been cancelled"))
    yield ()

  private def clientHold(server: TCPServer): IO[Unit] =
    TCP
      .connect(server.address)
      .widen[EmileError]
      .use(socket => EffIO.liftF(socket.write(Chunk.array("hello".getBytes("UTF-8"))).absolve *> IO.never[Unit]))
      .absolve

end GracefulShutdownSpec
