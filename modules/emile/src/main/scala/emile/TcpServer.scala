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

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue
import fs2.Stream
import com.comcast.ip4s.GenSocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibuvPoller
import emile.unsafe.Routing

final private[emile] class TcpServerState(
  val handle: Ptr[Byte],
  val poller: LibuvPoller,
  val address: GenSocketAddress,
  val connections: UnboundedQueue[IO, Either[EmileError.Io, Unit]]
)

/** A listening TCP server, acquired through [[Tcp$ Tcp]]. Accept operations are on
  * [[TcpServer$ TcpServer]].
  */
opaque type TcpServer = TcpServerState

/** Accept operations, factories, and equality for [[TcpServer]]. */
object TcpServer:

  given CanEqual[TcpServer, TcpServer] = CanEqual.derived

  extension (server: TcpServer)

    /** The local address the server is bound to - captured at bind. */
    def address: GenSocketAddress = server.address

    /** Stream of accepted connections. Each emitted [[TcpSocket]] is implicitly resource-scoped:
      * the underlying socket is closed when the consumer's per-element scope ends. Pull-style
      * accept - libuv stops the listener watcher between `uv_connection_cb` and the next
      * `uv_accept`, so kernel backlog absorbs all slack between arrivals and pulls.
      */
    def connections: EmStream[EmileError.Io, TcpSocket] =
      Stream.resource(server.acceptOne).repeat

    /** A single accepted connection as a scoped resource - the socket closes when the resource's
      * scope ends.
      */
    def acceptOne: EmResource[EmileError.Io, TcpSocket] =
      Resource.make[EffIO.Of[EmileError.Io], TcpSocket](acceptNext(server))(socket => EffIO.liftF(TcpSocket.release(socket)))

  end extension

  /** Build the server state and store it in the listen handle's `data` slot so [[connectionCb]] can
    * recover it. Storing here, inside the companion, avoids an opaque-type cast at the call site.
    */
  private[emile] def construct(
    handle: Ptr[Byte],
    poller: LibuvPoller,
    address: GenSocketAddress,
    queue: UnboundedQueue[IO, Either[EmileError.Io, Unit]]
  ): TcpServer =
    val state = new TcpServerState(handle, poller, address, queue)
    CallbackBridge.store(poller, handle, state)
    state

  /** Server release: clear the bridge then `uv_close` and await. The connection queue is fed only
    * from the loop thread, so once the close completes there is no in-flight producer.
    */
  private[emile] def release(server: TcpServer): IO[Unit] =
    Routing
      .onOwner(server.poller)(CallbackBridge.clear(server.poller, server.handle))
      .flatMap(_ => Routing.closeHandle(server.poller, server.handle))

  /** `uv_connection_cb` - run on the server's loop thread. Recovers the server state through the
    * handle's `data` slot and offers the connection signal to the queue. A negative status is an
    * exotic libuv connection-acceptance failure (e.g. `EMFILE` after the `uv__emfile_trick`
    * exhausts) - surface it on the same queue so the next pull sees it.
    */
  private[emile] val connectionCb: LibUV.ConnectionCB = (handle: Ptr[Byte], status: CInt) =>
    val state = CallbackBridge.load[TcpServerState](handle)
    val signal: Either[EmileError.Io, Unit] = if status < 0 then Left(IoMapping.fromCode(status)) else Right(())
    state.connections.unsafeOffer(signal)

  private def acceptNext(server: TcpServer): EmIO[EmileError.Io, TcpSocket] =
    EffIO.lift(
      server.connections.take.flatMap:
        case Left(error) => IO.pure(Left(error))
        case Right(()) => Routing.onOwner(server.poller)(performAccept(server))
    )

  // FFI: client handle calloc with throw-on-OOM, uv_close cleanup of a half-built client.
  // scalafix:off DisableSyntax

  private def performAccept(server: TcpServer): Either[EmileError.Io, TcpSocket] =
    val client = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_TCP))
    if client == null then throw new OutOfMemoryError("emile: uv_tcp_t allocation failed")
    val initRc = LibUV.uv_tcp_init(server.poller.loop, client)
    if initRc != 0 then
      stdlib.free(client)
      Left(IoMapping.fromCode(initRc))
    else
      val acceptRc = LibUV.uv_accept(server.handle, client)
      if acceptRc != 0 then
        LibUV.uv_close(client, freeHandleCb)
        Left(IoMapping.fromCode(acceptRc))
      else
        captureAddresses(client) match
          case Left(error) =>
            LibUV.uv_close(client, freeHandleCb)
            Left(error)
          case Right((local, peer)) =>
            Right(TcpSocket.construct(client, server.poller, local, peer))
    end if
  end performAccept

  private def captureAddresses(handle: Ptr[Byte]): Either[EmileError.Io, (GenSocketAddress, GenSocketAddress)] =
    for
      local <- TcpSocket.localAddressOf(handle).left.map(toIoError)
      peer <- TcpSocket.peerAddressOf(handle).left.map(toIoError)
    yield (local, peer)

  private def toIoError(rc: Int): EmileError.Io =
    if rc == 0 then EmileError.Io.Unexpected(new IllegalStateException("emile: unsupported TCP address family"))
    else IoMapping.fromCode(rc)

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end TcpServer
