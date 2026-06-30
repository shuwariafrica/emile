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

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.scalanative.posix.unistd

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk

/** Covers [[IPC.bind]] / [[IPC.connect]] / [[StreamServer.accepted]] and the [[IPCSocket]] surface
  * end-to-end over a Unix-domain round-trip - hermetic (no loopback, no filesystem entry) via the
  * Linux abstract namespace, plus the filesystem path, address round-trip, peer credentials, and
  * socket-file chmod.
  */
final class IPCSpec extends EmileSuite:

  private val nameCounter = new AtomicInteger(0)

  // A unique abstract name per bind, without java.util.UUID (its SecureRandom is unported on Native):
  // this process's pid plus a per-suite counter.
  private def uniqueName: String = s"emile-ipcspec-${unistd.getpid()}-${nameCounter.incrementAndGet()}"

  private def uniqueAbstract: IPCAddress = IPCAddress.Abstract(uniqueName)

  test("abstract-namespace server-client echo via read + write") {
    val payload: Chunk[Byte] = Chunk.array("hello, ipc!".getBytes("UTF-8"))
    IPC
      .bind(uniqueAbstract)
      .widen[EmileError]
      .use(server => EffIO.liftF(echoRoundTrip(server, payload)))
      .absolve
      .timeout(5.seconds)
  }

  test("bind reports the bound abstract address") {
    val name = uniqueName
    IPC
      .bind(IPCAddress.Abstract(name))
      .widen[EmileError]
      .use(server => EffIO.suspend(assertEquals(server.address, IPCAddress.Abstract(name))))
      .absolve
      .timeout(5.seconds)
  }

  test("filesystem-path server streams the payload through a pipe echo") {
    val payload: Chunk[Byte] = Chunk.array("streaming-ipc-payload".getBytes("UTF-8"))
    tempSocketPath
      .use(path =>
        IPC
          .bind(IPCAddress.Path(path))
          .widen[EmileError]
          .use(server => EffIO.liftF(streamingEcho(server, payload)))
          .absolve
      )
      .timeout(5.seconds)
  }

  test("peerCredentials reports the connected peer's process identity") {
    IPC
      .bind(uniqueAbstract)
      .widen[EmileError]
      .use(server => EffIO.liftF(checkPeerCredentials(server)))
      .absolve
      .timeout(5.seconds)
  }

  test("chmod sets the socket-file access mode on a filesystem-path server") {
    tempSocketPath
      .use(path =>
        IPC
          .bind(IPCAddress.Path(path))
          .widen[EmileError]
          .use(server => server.chmod(IPCMode.ReadWrite))
          .absolve
      )
      .timeout(5.seconds)
  }

  private def echoRoundTrip(server: IPCServer, payload: Chunk[Byte]): IO[Unit] =
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
      IPC
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

  private def streamingEcho(server: IPCServer, payload: Chunk[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      server.accepted
        .evalMap(_.use(socket => socket.reads.through(socket.writes).compile.drain.flatMap(_ => socket.endOfOutput)))
        .head
        .compile
        .drain
        .absolve

    val cliWork: IO[Unit] =
      IPC
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

  private def checkPeerCredentials(server: IPCServer): IO[Unit] =
    // The peer of the client socket is this very process (the server listens here), so the reported
    // process and user ids must be this process's own.
    val srvWork: IO[Unit] =
      server.accepted.evalMap(_.use(_ => EffIO.liftF(IO.sleep(500.millis)))).head.compile.drain.absolve

    val cliWork: IO[Unit] =
      IPC
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF(
            socket.peerCredentials.absolve.flatMap(creds =>
              IO:
                assertEquals(creds.processId, unistd.getpid())
                assertEquals(creds.userId, unistd.getuid().toInt)
            )
          )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end checkPeerCredentials

  private def tempSocketPath: Resource[IO, String] =
    Resource
      .make(IO(Files.createTempDirectory("emile-ipcspec")))(dir => IO(deleteDir(dir)))
      .map(dir => dir.resolve("test.sock").toString)

  // libuv unlinks the bound socket file on release, so the directory is normally empty by now; remove
  // any residue and the directory itself.
  private def deleteDir(dir: Path): Unit =
    Files.deleteIfExists(dir.resolve("test.sock")): Unit
    Files.deleteIfExists(dir): Unit

end IPCSpec
