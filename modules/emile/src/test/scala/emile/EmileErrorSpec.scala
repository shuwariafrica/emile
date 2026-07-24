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
import boilerplate.effect.RetryPolicy
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// Each code-dispatch table is exercised by driving a real libuv operation to the failure it maps,
// rather than by asserting the mapping against itself. The Unexpected collapsing constructors and the
// SignalNumber / LoopConfig helpers have no suite of their own and are covered here.
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

  test("a post-connect option failure keeps its typed IO error, not buried under Connect") {
    // A sub-second keep-alive is rejected by the post-connect option step (an IO.InvalidArgument) after
    // the connect itself succeeds; the union channel keeps it typed as IO, not collapsed to Connect.
    val badOptions = TCPOptions.default.copy(keepAlive = Some(TCPKeepAlive(500.millis, 500.millis, 9)))
    TCP
      .bind(anyLoopback)
      .use(server =>
        EffIO.liftF(
          TCP.connect(server.address, badOptions).use(_ => EffIO.succeed(())).either.map {
            case Left(EmileError.IO.InvalidArgument(_)) => ()
            case other => fail(s"expected IO.InvalidArgument, got: $other")
          }
        )
      )
      .absolve
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

  test("a channel narrowed to payload-free cases reifies every arm") {
    // Each case is a class, so the union erases to a common supertype and every arm passes the test the
    // channel reifies. Singleton arms would erase to one of them, and the rest would surface as defects
    // rather than the typed errors they are.
    type Narrow = EmileError.IO.EndOfStream | EmileError.IO.ConnectionReset
    val endOfStream: EmIO[Narrow, Unit] = EffIO.fail(EmileError.IO.EndOfStream)
    val connectionReset: EmIO[Narrow, Unit] = EffIO.fail(EmileError.IO.ConnectionReset)
    for
      first <- endOfStream.either
      second <- connectionReset.either
    yield
      first match
        case Left(EmileError.IO.EndOfStream) => ()
        case other => fail(s"expected EndOfStream, got: $other")
      second match
        case Left(EmileError.IO.ConnectionReset) => ()
        case other => fail(s"expected ConnectionReset, got: $other")
  }

  test("read with a non-positive size fails with IO.InvalidArgument") {
    TCP
      .bind(anyLoopback)
      .use(server =>
        EffIO.liftF(connected(server): socket =>
          socket.read(0).either.map {
            case Left(EmileError.IO.InvalidArgument(_)) => ()
            case other => fail(s"expected InvalidArgument, got: $other")
          })
      )
      .absolve
      .timeout(5.seconds)
  }

  test("a write after endOfOutput fails with IO.ConflictingOperation") {
    val chunk: Chunk[Byte] = Chunk.array("x".getBytes("UTF-8"))
    TCP
      .bind(anyLoopback)
      .use(server =>
        EffIO.liftF(connected(server): socket =>
          socket.endOfOutput.absolve.flatMap(_ =>
            socket.write(chunk).either.map {
              case Left(EmileError.IO.ConflictingOperation) => ()
              case other => fail(s"expected ConflictingOperation, got: $other")
            }
          ))
      )
      .absolve
      .timeout(5.seconds)
  }

  test("Unexpected destructures its cause") {
    EmileError.IO.Unexpected(new RuntimeException("boom")) match
      case EmileError.IO.Unexpected(cause) => assertEquals(cause.getMessage, "boom")
      case other => fail(s"expected Unexpected, got: $other")
  }

  test("HostConnect.transient classifies every connect and DNS arm") {
    // Ephemeral conditions - a fresh attempt may succeed with no operator action.
    assert(EmileError.Connect.ConnectionRefused.transient)
    assert(EmileError.Connect.NetworkUnreachable.transient)
    assert(EmileError.Connect.HostUnreachable.transient)
    assert(EmileError.Connect.AddressNotAvailable.transient)
    assert(EmileError.Connect.TimedOut.transient)
    assert(EmileError.Connect.NotFound.transient)
    assert(EmileError.Connect.TooManyOpenFiles.transient)
    assert(EmileError.DNS.TemporaryFailure("host").transient)
    // Durable conditions - retrying cannot help.
    assert(!EmileError.Connect.PermissionDenied.transient)
    assert(!EmileError.Connect.InvalidAddress("bad").transient)
    assert(!EmileError.Connect.Unexpected(new RuntimeException("boom")).transient)
    assert(!EmileError.DNS.UnknownHost("host").transient)
    assert(!EmileError.DNS.System(ErrorCode(ErrorCode.UV_EAI_NONAME)).transient)
    assert(!EmileError.DNS.Unexpected(new RuntimeException("boom")).transient)
    // System(code): each transient libuv code classifies transient; any other code does not.
    assert(EmileError.Connect.System(ErrorCode(ErrorCode.UV_EAGAIN)).transient)
    assert(EmileError.Connect.System(ErrorCode(ErrorCode.UV_ECONNABORTED)).transient)
    assert(EmileError.Connect.System(ErrorCode(ErrorCode.UV_EINTR)).transient)
    assert(EmileError.Connect.System(ErrorCode(ErrorCode.UV_ENOBUFS)).transient)
    assert(EmileError.Connect.System(ErrorCode(ErrorCode.UV_ENOMEM)).transient)
    assert(EmileError.Connect.System(ErrorCode(ErrorCode.UV_ENETDOWN)).transient)
    assert(!EmileError.Connect.System(ErrorCode(ErrorCode.UV_ECONNREFUSED)).transient)
    // AllAddressesFailed derives from its member failures.
    assert(EmileError.Connect.AllAddressesFailed(NonEmptyList.of(EmileError.Connect.ConnectionRefused)).transient)
    assert(!EmileError.Connect.AllAddressesFailed(NonEmptyList.of(EmileError.Connect.PermissionDenied)).transient)
    val mixed = NonEmptyList.of[EmileError.Connect](EmileError.Connect.PermissionDenied, EmileError.Connect.TimedOut)
    assert(EmileError.Connect.AllAddressesFailed(mixed).transient)
  }

  test("a bounded retry reconnects once a listener appears on a refused port") {
    val policy = RetryPolicy.fullJitter(20.millis).withMaxAttempts(200).withMaxDelay(100.millis)
    def transientConnect(error: EmileError.Connect | EmileError.IO): Boolean = error match
      case hc: EmileError.HostConnect => hc.transient
      case _: EmileError.IO => false
    // A free loopback address - bind then release - so a connect is refused until a listener reappears.
    val freeAddress: IO[SocketAddress[IpAddress]] =
      TCP.bind(anyLoopback).use(server => EffIO.suspend(server.address)).absolve
    freeAddress.flatMap { addr =>
      val listener: IO[Unit] =
        IO.sleep(200.millis) *> TCP
          .bind(addr)
          .widen[EmileError]
          .use(server => server.accepted.head.evalMap(_.use(_ => EffIO.succeed(()))).compile.drain)
          .absolve
      val reconnect: IO[Unit] =
        EffIO.retry(TCP.connect(addr).use(_ => EffIO.succeed(())), policy, retryOn = transientConnect).absolve
      listener.background.use(_ => reconnect).timeout(20.seconds)
    }
  }

  // Connects a client to `server` (the kernel completes the handshake off the listener backlog, so no
  // server-side accept is needed) and runs `f` on the connected socket.
  private def connected(server: TCPServer)(f: TCPSocket => IO[Unit]): IO[Unit] =
    TCP.connect(server.address).use(socket => EffIO.liftF(f(socket))).absolve

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
