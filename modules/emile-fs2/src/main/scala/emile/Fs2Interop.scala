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

import java.net.StandardSocketOptions
import scala.concurrent.duration.DurationInt

import boilerplate.effect.EffIO
import cats.arrow.FunctionK
import cats.effect.IO
import fs2.Chunk
import fs2.Pipe
import fs2.Stream
import fs2.io.net.Socket as Fs2Socket
import fs2.io.net.SocketOption
import com.comcast.ip4s.GenSocketAddress
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.SocketAddress

/** fs2-io interop: adapt emile's typed-error [[TCPSocket]] / [[TCPServer]] handles to fs2's
  * `Socket[IO]` and `Stream[IO, Socket[IO]]`. The typed-to-Throwable projection happens through
  * `EffIO.absolve` - the deliberate, lossless relaxation point for code that consumes fs2's
  * `Throwable`-channel `Socket[F]` shape (`EmileError <: Exception`, so the value carried into the
  * channel is the same instance, recoverable / matchable).
  */
object Fs2Interop:

  extension (socket: TCPSocket)
    /** Adapt this [[TCPSocket]] to `fs2.io.net.Socket[IO]`. The fs2 socket's lifecycle stays bound
      * to the emile `Resource` scope the [[TCPSocket]] was acquired in.
      */
    def asFs2: Fs2Socket[IO] = new Fs2SocketAdapter(socket)

  extension (server: TCPServer)
    /** Adapt this [[TCPServer]]'s accepted-connection stream to the `Stream[IO, Socket[IO]]` shape
      * fs2's own server side yields - matches `ServerSocket.accept`.
      */
    def acceptFs2: Stream[IO, Fs2Socket[IO]] =
      server.accepted.translate(absolveIoK).flatMap(connection => Stream.resource(connection.mapK(absolveIoK)).map(_.asFs2))

  // Typed-error -> IO natural transformation; needed because Stream.translate takes a ~>, not absolve.
  private val absolveIoK: FunctionK[EffIO.Of[EmileError.IO], IO] =
    new FunctionK[EffIO.Of[EmileError.IO], IO]:
      def apply[A](fa: EffIO[EmileError.IO, A]): IO[A] = fa.absolve

  // Options the adapter forwards to a TCPSocket setter; getOption still returns None for them, as
  // emile exposes no corresponding getters.
  private val SupportedOptionKeys: Set[SocketOption.Key[?]] = Set(
    StandardSocketOptions.TCP_NODELAY,
    StandardSocketOptions.SO_KEEPALIVE
  )

  // fs2's SO_KEEPALIVE is a single boolean, but emile's keep-alive carries idle/interval/count, so
  // turning it on applies this 60-second profile.
  private val DefaultKeepAlive: TCPKeepAlive = TCPKeepAlive.simple(60.seconds)

  // Backs asFs2: forwards every method to the emile TCPSocket, absolving the typed-error channel onto
  // IO's Throwable channel.
  final private class Fs2SocketAdapter(socket: TCPSocket) extends Fs2Socket[IO]:

    def address: GenSocketAddress = socket.address
    def peerAddress: GenSocketAddress = socket.peerAddress

    def read(maxBytes: Int): IO[Option[Chunk[Byte]]] = socket.read(maxBytes).absolve
    def readN(numBytes: Int): IO[Chunk[Byte]] = socket.readN(numBytes).absolve
    def reads: Stream[IO, Byte] = socket.reads.translate(absolveIoK)

    def write(bytes: Chunk[Byte]): IO[Unit] = socket.write(bytes).absolve
    def writes: Pipe[IO, Byte, Nothing] = _.chunks.foreach(write)

    def endOfInput: IO[Unit] = socket.endOfInput.absolve
    def endOfOutput: IO[Unit] = socket.endOfOutput.absolve

    val supportedOptions: IO[Set[SocketOption.Key[?]]] = IO.pure(SupportedOptionKeys)

    def getOption[A](key: SocketOption.Key[A]): IO[Option[A]] = IO.pure(None)

    def setOption[A](key: SocketOption.Key[A], value: A): IO[Unit] =
      if key eq StandardSocketOptions.TCP_NODELAY then socket.setNoDelay(value.asInstanceOf[java.lang.Boolean]).absolve // scalafix:ok DisableSyntax
      else if key eq StandardSocketOptions.SO_KEEPALIVE then
        val cfg = if value.asInstanceOf[java.lang.Boolean] then Some(DefaultKeepAlive) else None // scalafix:ok DisableSyntax
        socket.setKeepAlive(cfg).absolve
      else IO.raiseError(new IllegalArgumentException(s"emile-fs2: socket option not supported: $key"))

    // Deprecated on fs2's Socket / SocketInfo but still abstract, so they must be implemented.

    def isOpen: IO[Boolean] = IO.pure(true)
    def localAddress: IO[SocketAddress[IpAddress]] = IO(socket.address)
    def remoteAddress: IO[SocketAddress[IpAddress]] = IO(socket.peerAddress)

  end Fs2SocketAdapter

end Fs2Interop
