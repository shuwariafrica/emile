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
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers [[Socket.sendFile]] over `uv_fs_sendfile`: a server streams a temp file to the peer via
  * the kernel and the client receives the content.
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

  private def sendFileRoundTrip(server: TcpServer, content: Array[Byte]): IO[Unit] =
    tempFile(content).use(path => sendFileEcho(server, path, content))

  private def sendFileEcho(server: TcpServer, path: Path, content: Array[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      OpenFile
        .open(path)
        .widen[EmileError]
        .use(file =>
          server.connections
            .evalMap(socket => socket.sendFile(file, 0L, content.length.toLong))
            .head
            .compile
            .drain
        )
        .absolve

    val cliWork: IO[Unit] =
      Tcp
        .connect(server.address.asIpUnsafe)
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

  private def tempFile(content: Array[Byte]): Resource[IO, Path] =
    Resource.make(IO(writeTempFile(content)))(file => IO(file.delete(): Unit)).map(_.toPath)

  private def writeTempFile(content: Array[Byte]): File =
    val file = File.createTempFile("emile-sendfile", ".tmp")
    val out = new FileOutputStream(file)
    try out.write(content)
    finally out.close()
    file

end TcpSendFileSpec
