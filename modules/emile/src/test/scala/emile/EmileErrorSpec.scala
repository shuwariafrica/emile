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
import scala.concurrent.duration.*
import scala.scalanative.posix.sys.stat
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.toCString
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Covers the [[EmileError]] vocabulary: each code-dispatch table is exercised by driving a real
  * libuv operation to the failure it maps, with the collapsing `Unexpected` constructors and the
  * [[SignalNumber]] / [[LoopConfig]] helpers this spec also houses.
  */
final class EmileErrorSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("binding an address already in use maps to Bind.AddressInUse") {
    TCP
      .bind(anyLoopback)
      .use(server =>
        EffIO.liftF(
          TCP.bind(server.address).use(_ => EffIO.succeed(())).either.map {
            case Left(EmileError.Bind.AddressInUse) => ()
            case other => fail(s"expected AddressInUse, got: $other")
          }
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("connecting to a released port maps to Connect.ConnectionRefused") {
    val closedAddress: IO[SocketAddress[IpAddress]] =
      TCP.bind(anyLoopback).use(server => EffIO.suspend(server.address)).absolve
    closedAddress
      .flatMap(addr =>
        TCP.connect(addr).use(_ => EffIO.succeed(())).either.map {
          case Left(EmileError.Connect.ConnectionRefused) => ()
          case other => fail(s"expected ConnectionRefused, got: $other")
        }
      )
      .timeout(5.seconds)
  }

  test("readN past a peer half-close maps to IO.EndOfStream") {
    val greeting: Chunk[Byte] = Chunk.array("hi".getBytes("UTF-8"))
    TCP
      .bind(anyLoopback)
      .use(server => EffIO.liftF(shortReadRoundTrip(server, greeting)))
      .absolve
      .timeout(5.seconds)
  }

  test("opening a missing file maps to IO.NotFound") {
    tempDir
      .use(dir =>
        OpenFile.open(dir.resolve("does-not-exist")).use(_ => EffIO.succeed(())).either.map {
          case Left(EmileError.IO.NotFound) => ()
          case other => fail(s"expected IO.NotFound, got: $other")
        }
      )
      .timeout(5.seconds)
  }

  test("opening an unreadable file maps to IO.PermissionDenied") {
    unreadableFile
      .use(file =>
        OpenFile.open(file).use(_ => EffIO.succeed(())).either.map {
          case Left(EmileError.IO.PermissionDenied) => ()
          case other => fail(s"expected IO.PermissionDenied, got: $other")
        }
      )
      .timeout(5.seconds)
  }

  test("connecting to a missing socket path maps to Connect.NotFound") {
    tempDir
      .use(dir =>
        IPC.connect(IPCAddress.Path(dir.resolve("missing.sock").toString)).use(_ => EffIO.succeed(())).either.map {
          case Left(EmileError.Connect.NotFound) => ()
          case other => fail(s"expected Connect.NotFound, got: $other")
        }
      )
      .timeout(5.seconds)
  }

  test("binding under a missing directory maps to Bind.PermissionDenied") {
    // uv_pipe_bind2 maps a missing-parent ENOENT to EACCES for Windows parity, so this surfaces as
    // PermissionDenied, not NotFound.
    tempDir
      .use(dir =>
        IPC.bind(IPCAddress.Path(dir.resolve("absent").resolve("s.sock").toString)).use(_ => EffIO.succeed(())).either.map {
          case Left(EmileError.Bind.PermissionDenied) => ()
          case other => fail(s"expected Bind.PermissionDenied, got: $other")
        }
      )
      .timeout(5.seconds)
  }

  test("Unexpected returns an already-typed cause unwrapped") {
    assertEquals(EmileError.Bind.Unexpected(EmileError.Bind.PermissionDenied), EmileError.Bind.PermissionDenied: EmileError.Bind)
    assertEquals(EmileError.IO.Unexpected(EmileError.IO.ConnectionReset), EmileError.IO.ConnectionReset: EmileError.IO)
  }

  test("Unexpected wraps a foreign throwable with a derived message") {
    assertEquals(EmileError.Bind.Unexpected(new RuntimeException("boom")).getMessage, "Unexpected bind failure: boom")
  }

  test("SignalNumber rejects out-of-range numbers and accepts the boundary values") {
    assert(SignalNumber.from(0).isLeft)
    assert(SignalNumber.from(65).isLeft)
    assert(SignalNumber.from(1).isRight)
    assert(SignalNumber.from(64).isRight)
  }

  test("LoopConfig presets") {
    assert(!LoopConfig.default.blockProfilerSignal)
    assert(!LoopConfig.default.useIoUringSqpoll)
    assert(LoopConfig.profilerProfile.blockProfilerSignal)
  }

  test("System derives its message from the libuv error code") {
    val message = EmileError.Bind.System(ErrorCode(ErrorCode.UV_EADDRINUSE)).getMessage
    assert(message.startsWith("EADDRINUSE"), message)
  }

  private def shortReadRoundTrip(server: TCPServer, greeting: Chunk[Byte]): IO[Unit] =
    val srvWork: IO[Unit] =
      server.accepted
        .evalMap(_.use(socket => socket.write(greeting).flatMap(_ => socket.endOfOutput)))
        .head
        .compile
        .drain
        .absolve

    val cliWork: IO[Unit] =
      TCP
        .connect(server.address)
        .use(socket =>
          EffIO.liftF(
            socket.readN(greeting.size + 1).either.map {
              case Left(EmileError.IO.EndOfStream) => ()
              case other => fail(s"expected EndOfStream, got: $other")
            }
          )
        )
        .absolve

    srvWork.background.use(_ => cliWork)
  end shortReadRoundTrip

  private def tempDir: Resource[IO, Path] =
    Resource.make(IO(Files.createTempDirectory("emile-errorspec")))(dir => IO(Files.deleteIfExists(dir): Unit))

  // Mode 000: unreadable to the non-root test process, so open reports EACCES.
  private def unreadableFile: Resource[IO, Path] =
    Resource.make(IO {
      val file = Files.createTempFile("emile-errorspec-", ".secret")
      denyAll(file)
      file
    })(file => IO(Files.deleteIfExists(file): Unit))

  private def denyAll(file: Path): Unit =
    Zone:
      val _ = stat.chmod(toCString(file.toString), 0.toUInt)

end EmileErrorSpec
