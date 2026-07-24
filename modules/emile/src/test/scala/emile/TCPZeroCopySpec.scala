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
import scala.util.control.NoStackTrace

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// A codec error naming no emile type, with class arms - the borrowed read carries it as the E arm of
// the union channel EmileError.IO | CodecErr, and the union reifies (class arms, not singleton types).
sealed abstract class CodecErr(message: String) extends Exception(message) with NoStackTrace derives CanEqual
object CodecErr:
  final case class ShortHeader(have: Int) extends CodecErr(s"short header: $have bytes")

// The borrowed-byte socket surface - the Slice-callback read, write(slice), write(slices),
// tryWrite(slice) - and the reusable write-scratch recipe, over a loopback round-trip.
final class TCPZeroCopySpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("a borrowed read feeding write round-trips the payload with no copy") {
    val payload: Chunk[Byte] = Chunk.array("zero-copy emile".getBytes("UTF-8"))
    roundTrip(
      server => acceptOne(server)(socket => socket.read((slice: Slice) => socket.write(slice)).absolve.void),
      server =>
        connect(server)(socket =>
          for
            _ <- socket.write(payload).absolve
            echo <- socket.read(payload.size).absolve
            _ <- IO(assertEquals(echo.fold(List.empty[Byte])(_.toList), payload.toList))
          yield ()
        )
    )
  }

  test("tryWrite synchronously writes a small payload and reports its byte count") {
    val payload: Chunk[Byte] = Chunk.array("try-write".getBytes("UTF-8"))
    // A small loopback payload fits the send buffer, so the synchronous write reports the whole chunk.
    roundTrip(
      server =>
        acceptOne(server)(socket =>
          socket.read((slice: Slice) => socket.tryWrite(slice).map(written => assertEquals(written, slice.length))).absolve.void
        ),
      server =>
        connect(server)(socket =>
          for
            _ <- socket.write(payload).absolve
            echo <- socket.read(payload.size).absolve
            _ <- IO(assertEquals(echo.fold(List.empty[Byte])(_.toList), payload.toList))
          yield ()
        )
    )
  }

  test("a borrowed read decodes a header through the bulk reader on a EmileError.IO | CodecErr channel") {
    val header: Chunk[Byte] = Chunk.array(Array[Byte](0, 0, 0, 42))
    roundTrip(
      server => acceptOne(server)(socket => socket.write(header).absolve.void),
      server =>
        connect(server)(socket =>
          val decoded: EmIO[EmileError.IO | CodecErr, Option[Int]] =
            socket.read((slice: Slice) =>
              if slice.length < 4 then EffIO.fail(CodecErr.ShortHeader(slice.length))
              else EffIO.succeed(slice.readBE[Int](0))
            )
          decoded.either.map {
            case Right(Some(42)) => ()
            case other => fail(s"expected Right(Some(42)), got: $other")
          }
        )
    )
  }

  test("a borrowed read copies bytes out to persist them past the callback") {
    val payload: Chunk[Byte] = Chunk.array("escape-me".getBytes("UTF-8"))
    roundTrip(
      server => acceptOne(server)(socket => socket.write(payload).absolve.void),
      server =>
        connect(server)(socket =>
          for
            escaped <- socket.read((slice: Slice) => EffIO.succeed(slice.toArray)).absolve
            _ <- IO(assertEquals(escaped.fold(List.empty[Byte])(_.toList), payload.toList))
          yield ()
        )
    )
  }

  test("write(slice) reuses one array-backed scratch across writes, completion before reuse") {
    // One codec-owned array, framed per fill; each write completes before the next fill (the monad
    // sequences it), so reuse never races an in-flight write.
    roundTrip(
      server =>
        acceptOne(server)(socket =>
          socket.readN(10).absolve.flatMap(chunk => IO(assertEquals(new String(chunk.toArray, "UTF-8"), "alphabeta!")))
        ),
      server =>
        connect(server) { socket =>
          val scratch = new Array[Byte](16)
          def fillAndWrite(msg: String): EmIO[EmileError.IO, Unit] =
            EffIO
              .suspend {
                val bytes = msg.getBytes("UTF-8")
                System.arraycopy(bytes, 0, scratch, 0, bytes.length)
                bytes.length
              }
              .flatMap(n => socket.write(Slice.of(scratch, 0, n)))
          fillAndWrite("alpha").flatMap(_ => fillAndWrite("beta!")).absolve
        }
    )
  }

  test("write(slices) gathers several borrowed regions in order as one write") {
    val parts = List("gathered-", "borrowed-", "slices").map(_.getBytes("UTF-8"))
    val total = parts.map(_.length).sum
    roundTrip(
      server =>
        acceptOne(server)(socket =>
          socket.readN(total).absolve.flatMap(chunk => IO(assertEquals(new String(chunk.toArray, "UTF-8"), "gathered-borrowed-slices")))
        ),
      server => connect(server)(socket => socket.write(parts.map(Slice.of)).absolve)
    )
  }

  test("tryWrite then write(slice.drop(n)) composes the remainder") {
    val payload = "remainder-composition".getBytes("UTF-8")
    roundTrip(
      server =>
        acceptOne(server)(socket =>
          socket.readN(payload.length).absolve.flatMap(chunk => IO(assertEquals(new String(chunk.toArray, "UTF-8"), "remainder-composition")))
        ),
      server =>
        connect(server) { socket =>
          val slice = Slice.of(payload)
          socket.tryWrite(slice).flatMap(n => socket.write(slice.drop(n))).absolve
        }
    )
  }

  private def acceptOne(server: TCPServer)(handler: TCPSocket => IO[Unit]): IO[Unit] =
    server.accepted.evalMap(_.use(socket => EffIO.liftF(handler(socket)))).head.compile.drain.absolve

  private def connect(server: TCPServer)(body: TCPSocket => IO[Unit]): IO[Unit] =
    TCP.connect(server.address).widen[EmileError].use(socket => EffIO.liftF(body(socket))).absolve

  private def roundTrip(serverWork: TCPServer => IO[Unit], clientWork: TCPServer => IO[Unit]): IO[Unit] =
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server => EffIO.liftF(serverWork(server).background.use(_ => clientWork(server))))
      .absolve
      .timeout(5.seconds)

end TCPZeroCopySpec
