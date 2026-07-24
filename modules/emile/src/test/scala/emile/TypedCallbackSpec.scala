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

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// A pure application error naming no emile type - the union-channel callbacks carry it as the E arm.
enum AppError extends Exception derives CanEqual:
  case Rejected
  case Malformed(at: Int)

// The callback surface carrying an application's own error, which surfaces on the union channel
// EmileError.IO | AppError as the AppError arm while emile's own errors stay on the IO arm.
final class TypedCallbackSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  private val payload: Chunk[Byte] = Chunk.array("typed".getBytes("UTF-8"))

  test("onLoop surfaces a synchronous application error on the union channel, and a success value") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.liftF(
          TCP
            .connect(server.address)
            .widen[EmileError]
            .use(socket =>
              EffIO.liftF(
                for
                  failed <- socket.onLoop[AppError, Int](Left(AppError.Rejected)).either
                  succeeded <- socket.onLoop[AppError, Int](Right(7)).either
                  _ <- IO(assertEquals(failed, Left(AppError.Rejected): Either[EmileError.IO | AppError, Int]))
                  _ <- IO(assertEquals(succeeded, Right(7): Either[EmileError.IO | AppError, Int]))
                yield ()
              )
            )
            .absolve
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("consume surfaces the application's own error on the union channel") {
    runReading(socket => socket.consume((_: Slice) => Left(AppError.Malformed(1))).either)(AppError.Malformed(1))
  }

  test("the borrowed read threads the application error from its callback onto the union channel") {
    runReading(socket => socket.read((_: Slice) => EffIO.fail(AppError.Rejected)).either.map(_.map(_ => ())))(AppError.Rejected)
  }

  // Serves one chunk from the peer, then runs `read` on the client and asserts the expected union error.
  private def runReading(read: TCPSocket => IO[Either[EmileError.IO | AppError, Unit]])(expected: EmileError.IO | AppError): IO[Unit] =
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use(server =>
        EffIO.liftF(
          feedOneChunk(server).background.use(_ =>
            TCP
              .connect(server.address)
              .widen[EmileError]
              .use(socket =>
                EffIO.liftF(read(socket).map(result => assertEquals(result, Left(expected): Either[EmileError.IO | AppError, Unit])))
              )
              .absolve
          )
        )
      )
      .absolve
      .timeout(5.seconds)

  private def feedOneChunk(server: TCPServer): IO[Unit] =
    server.accepted
      .evalMap(_.use(socket => socket.write(payload).flatMap(_ => EffIO.liftF(IO.never[Unit]))))
      .head
      .compile
      .drain
      .absolve

end TypedCallbackSpec
