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

import java.io.File
import java.io.FileOutputStream
import java.nio.file.Path
import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers a server streaming a temp file to the peer two ways: [[Socket.sendFile]] (kernel
  * `uv_fs_sendfile`) and the backpressure-correct `OpenFile.reads.through(socket.writes)`.
  */
final class TcpSendFileSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("sendFile transfers the file content to the peer") {
    val content: Array[Byte] = "emile sendFile probe payload".getBytes("UTF-8")
    val program: IO[Unit] =
      Tcp
        .bind(anyLoopback)
        .widen[EmileError]
        .use(server => EffIO.liftF(sendFileRoundTrip(server, content)))
        .absolve
    program.timeout(5.seconds)
  }

  test("OpenFile.reads through a socket's writes streams the file body to the peer") {
    val content: Array[Byte] = "emile reads-through-writes whole-body probe payload".getBytes("UTF-8")
    val program: IO[Unit] =
      Tcp
        .bind(anyLoopback)
        .widen[EmileError]
        .use(server => EffIO.liftF(readsThroughRoundTrip(server, content)))
        .absolve
    program.timeout(5.seconds)
  }

  private def sendFileRoundTrip(server: TcpServer, content: Array[Byte]): IO[Unit] =
    tempFile(content).use(path => sendFileEcho(server, path, content))

  private def sendFileEcho(server: TcpServer, path: Path, content: Array[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      OpenFile
        .open(path)
        .widen[EmileError]
        .use(file =>
          server.accepted
            .evalMap(_.use(socket => socket.sendFile(file, 0L, content.length.toLong)))
            .head
            .compile
            .drain
        )
        .absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF(
            for
              chunk <- socket.readN(content.length).absolve
              _ <- IO(assertEquals(chunk.toList, content.toList))
            yield ()
          )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end sendFileEcho

  private def readsThroughRoundTrip(server: TcpServer, content: Array[Byte]): IO[Unit] =
    tempFile(content).use(path => readsThroughEcho(server, path, content))

  private def readsThroughEcho(server: TcpServer, path: Path, content: Array[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      OpenFile
        .open(path)
        .widen[EmileError]
        .use(file =>
          server.accepted
            .evalMap(_.use(socket => file.reads.through(socket.writes).compile.drain.flatMap(_ => socket.endOfOutput)))
            .head
            .compile
            .drain
        )
        .absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          EffIO.liftF(
            for
              echo <- socket.reads.compile.to(Chunk).absolve
              _ <- IO(assertEquals(echo.toList, content.toList))
            yield ()
          )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end readsThroughEcho

  test("sendFile concurrent with an in-flight write fails fast with ConflictingTransfer") {
    val content: Array[Byte] = "emile conflict probe".getBytes("UTF-8")
    tempFile(content)
      .use(path =>
        Tcp
          .bind(anyLoopback)
          .widen[EmileError]
          .use(server => EffIO.liftF(conflictEcho(server, path, content)))
          .absolve
      )
      .timeout(10.seconds)
  }

  private def conflictEcho(server: TcpServer, path: Path, content: Array[Byte]): IO[Unit] =
    // A payload far larger than the socket buffers, sent to a peer that never reads, keeps the uv_write
    // in flight (its callback fires only once every byte drains), so a concurrent sendFile observes it.
    val big: Chunk[Byte] = Chunk.array(new Array[Byte](32 * 1024 * 1024))
    val srvWork: IO[Unit] =
      server.accepted.evalMap(_.use(_ => EffIO.liftF(IO.sleep(3.seconds)))).head.compile.drain.absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          OpenFile
            .open(path)
            .widen[EmileError]
            .use(file =>
              EffIO.liftF(
                for
                  writer <- socket.write(big).absolve.start
                  _ <- IO.sleep(200.millis)
                  result <- socket.sendFile(file, 0L, content.length.toLong).either
                  _ <- writer.cancel
                  _ <- IO(result match
                         case Left(EmileError.Io.ConflictingTransfer) => ()
                         case other => fail(s"expected ConflictingTransfer, got: $other"))
                yield ()
              )
            )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end conflictEcho

  test("sendFile after a half-close fails fast with ConflictingTransfer") {
    val content: Array[Byte] = "emile half-close conflict probe".getBytes("UTF-8")
    tempFile(content)
      .use(path =>
        Tcp
          .bind(anyLoopback)
          .widen[EmileError]
          .use(server => EffIO.liftF(halfCloseConflictEcho(server, path, content)))
          .absolve
      )
      .timeout(10.seconds)
  }

  private def halfCloseConflictEcho(server: TcpServer, path: Path, content: Array[Byte]): IO[Unit] =
    // The server holds the connection open so the client's half-close, then sendFile, both run against a
    // live peer. endOfOutput's FIN would truncate a raw-fd sendFile, so the half-close terminally excludes
    // it: a later sendFile must fail fast rather than send bytes after the FIN.
    val srvWork: IO[Unit] =
      server.accepted.evalMap(_.use(_ => EffIO.liftF(IO.sleep(2.seconds)))).head.compile.drain.absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address)
        .widen[EmileError]
        .use(socket =>
          OpenFile
            .open(path)
            .widen[EmileError]
            .use(file =>
              EffIO.liftF(
                for
                  _ <- socket.endOfOutput.absolve
                  result <- socket.sendFile(file, 0L, content.length.toLong).either
                  _ <- IO(result match
                         case Left(EmileError.Io.ConflictingTransfer) => ()
                         case other => fail(s"expected ConflictingTransfer, got: $other"))
                yield ()
              )
            )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end halfCloseConflictEcho

  private def tempFile(content: Array[Byte]): Resource[IO, Path] =
    Resource.make(IO(writeTempFile(content)))(file => IO(file.delete(): Unit)).map(_.toPath)

  private def writeTempFile(content: Array[Byte]): File =
    val file = File.createTempFile("emile-sendfile", ".tmp")
    val out = new FileOutputStream(file)
    try out.write(content)
    finally out.close()
    file

end TcpSendFileSpec
