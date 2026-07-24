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

// Covers TCP.bindPerWorker: replicated per-worker listeners sharing a port through SO_REUSEPORT, their
// merged accept surface, the global serve limit, all-or-nothing acquire, and the registry lifecycle.
final class BindPerWorkerSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  private val payload: Chunk[Byte] = Chunk.array("bindPerWorker-emile".getBytes("UTF-8"))
  private val hello: Chunk[Byte] = Chunk.array("hello".getBytes("UTF-8"))

  test("bindPerWorker distributes a connection storm exactly-once across listeners") {
    val n = 200
    TCP
      .bindPerWorker(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use { pool =>
        EffIO.liftF(
          for
            shutdown <- IO.deferred[Unit]
            accepted <- Ref[IO].of(0)
            loopCounts <- Ref[IO].of(Map.empty[String, Int])
            handler =
              (socket: TCPSocket) =>
                socket
                  .onLoop(Right(Thread.currentThread().getName))
                  .flatMap(name => EffIO.liftF(loopCounts.update(m => m.updated(name, m.getOrElse(name, 0) + 1)) *> accepted.update(_ + 1)))
            serveFib <- pool.serve(64, shutdown.get)(handler)(_ => IO.unit).absolve.start
            _ <- Stream.emits(0 until n).covary[IO].parEvalMapUnordered(32)(_ => connectClose(pool.addresses.head)).compile.drain
            _ <- waitUntil(accepted.get.map(_ >= n))
            _ <- shutdown.complete(())
            _ <- serveFib.join.timeout(20.seconds)
            total <- accepted.get
            counts <- loopCounts.get
            _ <- IO.println(s"bindPerWorker connect-storm: $total accepts over ${pool.size} listeners, ${counts.size} accepted: $counts")
            _ <- IO(assertEquals(total, n))
            _ <- IO(if pool.size > 1 then assert(counts.size > 1, s"expected more than one listener to accept, got ${counts.size}: $counts"))
          yield ()
        )
      }
      .absolve
      .timeout(60.seconds)
  }

  test("bindPerWorker acquire is all-or-nothing: a typed Bind and no leaked listener") {
    for
      outcome <- TCP
                   .bind(anyLoopback, TCPOptions.default)
                   .widen[EmileError]
                   .use(plain =>
                     EffIO.liftF(
                       TCP
                         .bindPerWorker(plain.address, TCPOptions.default)
                         .use(_ => EffIO.succeed(()))
                         .either
                         .map(result => (port = plain.address, result = result))
                     )
                   )
                   .absolve
      _ <- IO(outcome.result match
             case Left(_: EmileError.Bind.AddressInUse) => ()
             case other => fail(s"expected Bind.AddressInUse at acquire, got $other"))
      // With the incumbent released, the port rebinds only if bindPerWorker leaked no listener still holding it.
      _ <- TCP
             .bind(outcome.port, TCPOptions.default)
             .widen[EmileError]
             .use(fresh => EffIO.suspend(assertEquals(fresh.address.port, outcome.port.port)))
             .absolve
    yield ()
  }

  test("bindPerWorker serve bounds concurrent handlers globally across listeners") {
    val maxConcurrent = 2
    val clients = 8
    TCP
      .bindPerWorker(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use { pool =>
        EffIO.liftF(
          for
            release <- IO.deferred[Unit]
            current <- Ref[IO].of(0)
            peak <- Ref[IO].of(0)
            started <- Ref[IO].of(0)
            shutdown <- IO.deferred[Unit]
            handler = (_: TCPSocket) =>
                        EffIO.liftF(
                          current.updateAndGet(_ + 1).flatMap(live => peak.update(_.max(live))) *>
                            started.update(_ + 1) *>
                            release.get *>
                            current.update(_ - 1)
                        )
            serveFib <- pool.serve(maxConcurrent, shutdown.get)(handler)(_ => IO.unit).absolve.start
            clientsFib <- Stream
                            .emits(0 until clients)
                            .covary[IO]
                            .parEvalMapUnordered(clients)(_ => clientHold(pool.addresses.head))
                            .compile
                            .drain
                            .start
            _ <- waitUntil(started.get.map(_ >= maxConcurrent))
            _ <- IO.sleep(300.millis)
            observedPeak <- peak.get
            _ <- IO(assert(observedPeak <= maxConcurrent, s"peak concurrent handlers $observedPeak exceeded the global limit $maxConcurrent"))
            _ <- IO(assert(observedPeak >= 1, s"expected at least one handler to run, got $observedPeak"))
            _ <- release.complete(())
            _ <- shutdown.complete(())
            _ <- serveFib.join.timeout(20.seconds)
            _ <- clientsFib.cancel
          yield ()
        )
      }
      .absolve
      .timeout(40.seconds)
  }

  test("bindPerWorker serve round-trips a payload and drains an in-flight handler on shutdown") {
    TCP
      .bindPerWorker(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use { pool =>
        EffIO.liftF(
          for
            started <- IO.deferred[Unit]
            completed <- Ref[IO].of(false)
            shutdown <- IO.deferred[Unit]
            handler = (socket: TCPSocket) =>
                        EffIO.liftF(started.complete(()).attempt.void *> echoOnce(socket) *> IO.sleep(500.millis) *> completed.set(true))
            serveFib <- pool.serve(8, shutdown.get)(handler)(_ => IO.unit).absolve.start
            echoed <- roundTrip(pool.addresses.head)
            _ <- IO(assertEquals(echoed, payload.toList))
            _ <- started.get
            _ <- shutdown.complete(())
            _ <- serveFib.join.timeout(20.seconds)
            done <- completed.get
            _ <- IO(assert(done, "the in-flight handler should have drained to completion, not been cancelled"))
          yield ()
        )
      }
      .absolve
      .timeout(40.seconds)
  }

  test("bindPerWorker serve wraps a handler defect as IO.Unexpected through the infallible twin") {
    TCP
      .bindPerWorker(anyLoopback, TCPOptions.server)
      .widen[EmileError]
      .use { pool =>
        EffIO.liftF(
          for
            shutdown <- IO.deferred[Unit]
            seen <- IO.deferred[EmileError.IO]
            handler = (_: TCPSocket) => EffIO.liftF(IO.raiseError[Unit](new RuntimeException("pool handler defect")))
            serveFib <- pool.serve(1, shutdown.get)(handler)(error => seen.complete(error).void).absolve.start
            _ <- connectClose(pool.addresses.head)
            observed <- seen.get.timeout(15.seconds)
            _ <- shutdown.complete(())
            _ <- serveFib.join.timeout(20.seconds)
            _ <- IO(observed match
                   case EmileError.IO.Unexpected(cause) => assertEquals(cause.getMessage, "pool handler defect")
                   case other => fail(s"expected IO.Unexpected, got $other"))
          yield ()
        )
      }
      .absolve
      .timeout(40.seconds)
  }

  test("runtime shutdown closes unreleased bindPerWorker listeners") {
    // Acquire the pool on a private runtime but drop its finaliser (leak the listeners): the runtime's
    // own shutdown must still close every listener on its poller, so shutdown returns rather than hanging.
    IO.blocking {
      val runtime = Emile.unsafeRuntime(LoopConfig.default)
      try TCP.bindPerWorker(anyLoopback, TCPOptions.server).widen[EmileError].allocated.absolve.map(_._1.size).unsafeRunSync()(using runtime)
      finally runtime.shutdown()
    }.map(size => assert(size >= 1, s"expected at least one listener on the pool, got $size"))
      .timeout(30.seconds)
  }

  private def connectClose(address: SocketAddress[IpAddress]): IO[Unit] =
    TCP.connect(address).widen[EmileError].use(_ => EffIO.succeed(())).absolve

  private def clientHold(address: SocketAddress[IpAddress]): IO[Unit] =
    TCP.connect(address).widen[EmileError].use(socket => EffIO.liftF(socket.write(hello).absolve *> IO.never[Unit])).absolve

  private def echoOnce(socket: TCPSocket): IO[Unit] =
    socket
      .read(payload.size)
      .absolve
      .flatMap:
        case Some(chunk) => socket.write(chunk).absolve
        case None => IO.unit

  private def roundTrip(address: SocketAddress[IpAddress]): IO[List[Byte]] =
    TCP
      .connect(address)
      .widen[EmileError]
      .use(socket => EffIO.liftF(socket.write(payload).absolve *> socket.read(payload.size).absolve.map(_.fold(List.empty[Byte])(_.toList))))
      .absolve

  private def waitUntil(cond: IO[Boolean]): IO[Unit] =
    cond.flatMap(done => if done then IO.unit else IO.sleep(20.millis) *> waitUntil(cond))

end BindPerWorkerSpec
