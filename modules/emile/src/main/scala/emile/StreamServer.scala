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

import scala.annotation.targetName
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue
import fs2.Stream

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibuvPoller
import emile.unsafe.Routing

final private[emile] class StreamServerState(
  val handle: Ptr[Byte],
  val poller: LibuvPoller,
  val address: Matchable,
  val connections: UnboundedQueue[IO, Either[EmileError.Io, Unit]],
  val acceptor: StreamServer.Acceptor
)

/** A listening stream server, parameterised by its [[SocketKind]] - the shared accept surface of
  * TCP and [[Ipc$ Ipc]] (Unix-domain / named-pipe) listeners. Covariant in `K`, so a [[TcpServer]]
  * is usable as an [[AnyServer]]. Accept operations are on [[StreamServer$ StreamServer]].
  */
opaque type StreamServer[+K <: SocketKind] = StreamServerState

/** A listening TCP server, acquired through [[Tcp$ Tcp]]. */
type TcpServer = StreamServer[SocketKind.Tcp]

/** A listening [[Ipc$ Ipc]] (Unix-domain / named-pipe) server, acquired through [[Ipc$ Ipc]]. */
type IpcServer = StreamServer[SocketKind.Ipc]

/** A stream server of any kind - the neutral spelling over which the shared operations resolve. */
type AnyServer = StreamServer[SocketKind]

/** Accept operations, the per-kind accept strategy, factories, and equality for [[StreamServer]].
  * The accept machinery is defined once over `StreamServer[K]` and shared by every kind; the
  * kind-specific steps (client-handle init, address capture, post-accept finish) are carried by the
  * [[Acceptor]] each transport builds at bind. Accepting on a `StreamServer[K]` yields a
  * `Socket[K]`, so the right kind-specific socket operations resolve.
  */
object StreamServer:

  given [K <: SocketKind] => CanEqual[StreamServer[K], StreamServer[K]] = CanEqual.derived

  inline def chmod(server: IpcServer, mode: IpcMode): EmIO[EmileError.Io, Unit] =
    server.chmod(mode)

  /** The per-kind accept strategy a [[StreamServer]] is built with - how to allocate, initialise,
    * and address a client handle, and the post-accept `finish` step (the TCP tuning; nothing for
    * Ipc). Built once at bind by the transport entry ([[Tcp$ Tcp]] / [[Ipc$ Ipc]]) and carried in
    * the server state, so the shared accept machinery stays kind-agnostic.
    */
  final private[emile] class Acceptor(
    val handleType: Int,
    val initClient: (Ptr[Byte], Ptr[Byte]) => CInt,
    val captureAddresses: Ptr[Byte] => Either[EmileError.Io, (Matchable, Matchable)],
    val finish: AnySocket => EmIO[EmileError.Io, Unit]
  )

  extension [K <: SocketKind](server: StreamServer[K])

    /** The local address the server is bound to, at the precise type for its kind - captured at
      * bind.
      */
    def address: AddressOf[K] =
      // phantom-guided: construct[K] stores exactly the AddressOf[K] for this kind.
      server.address.asInstanceOf[AddressOf[K]] // scalafix:ok DisableSyntax.asInstanceOf

    /** Stream of accept tokens, each a resource that accepts one connection when used, yielding a
      * `Socket[K]`. The socket's lifetime is the handler's `use` scope, so this is safe under every
      * combinator - concurrent `server.accepted.parEvalMapUnordered(n)(_.use(handle))` and serial
      * `server.accepted.evalMap(_.use(handle))` alike. Pull-style accept - libuv stops the listener
      * watcher between `uv_connection_cb` and the next `uv_accept`, so kernel backlog absorbs all
      * slack between arrivals and pulls.
      */
    def accepted: EmStream[EmileError.Io, EmResource[EmileError.Io, Socket[K]]] =
      Stream.emit(server.acceptOne).covary[EmIO.Of[EmileError.Io]].repeat

    /** A single accepted connection as a scoped resource - the socket closes when the resource's
      * scope ends. The kind's post-accept finish step (TCP tuning; nothing for Ipc) runs before the
      * socket is yielded.
      */
    def acceptOne: EmResource[EmileError.Io, Socket[K]] =
      // makeFull keeps the accept wait cancelable: Resource.make's acquire is uncancelable, which would
      // mask acceptNext's poll(take) and hang a cancelled idle accept (e.g. a graceful shutdown).
      Resource
        .makeFull[EffIO.Of[EmileError.Io], Socket[K]](poll => poll(acceptNext(server)))(socket => EffIO.liftF(Socket.release(socket)))
        .evalTap(socket => server.acceptor.finish(socket))

  end extension

  extension (server: IpcServer)

    /** Set the socket file's access mode via `uv_pipe_chmod`. A filesystem-path server only - an
      * abstract-namespace server has no socket file and yields a libuv error.
      */
    @targetName("ext_chmod")
    inline def chmod(mode: IpcMode): EmIO[EmileError.Io, Unit] =
      chmodPipe(server, mode)

  /** Build the server state of kind `K` and store it in the listen handle's `data` slot so
    * [[connectionCb]] can recover it. Storing here, inside the companion, avoids an opaque-type
    * cast at the call site.
    */
  private[emile] def construct[K <: SocketKind](
    handle: Ptr[Byte],
    poller: LibuvPoller,
    address: Matchable,
    connections: UnboundedQueue[IO, Either[EmileError.Io, Unit]],
    acceptor: Acceptor
  ): StreamServer[K] =
    val state = new StreamServerState(handle, poller, address, connections, acceptor)
    CallbackBridge.store(poller, handle, state)
    state

  /** Release the listener via [[Routing.closeHandle]] alone. `closeHandle` stores its own
    * completion over the handle's `data` slot and `uv_close`s it in one owner-thread step, which
    * stops any further `connection_cb`; a separate pre-clear would instead open a window where a
    * `connection_cb` reads a nulled slot and dereferences it across the C ABI.
    */
  private[emile] def release(server: AnyServer): IO[Unit] =
    Routing.closeHandle(server.poller, server.handle)

  /** `uv_connection_cb` - run on the server's loop thread. Recovers the server state through the
    * handle's `data` slot and offers the connection signal to the queue. A negative status is an
    * exotic libuv connection-acceptance failure (e.g. `EMFILE` after the `uv__emfile_trick`
    * exhausts) - surface it on the same queue so the next pull sees it.
    */
  private[emile] val connectionCb: LibUV.ConnectionCB = (handle: Ptr[Byte], status: CInt) =>
    val state = CallbackBridge.load[StreamServerState](handle)
    val signal: Either[EmileError.Io, Unit] = if status < 0 then Left(IoMapping.fromCode(status)) else Right(())
    state.connections.unsafeOffer(signal)

  // The wait is cancelable, but the take -> uv_accept transition is not: a cancel that has consumed a
  // connection signal must still run uv_accept, or the accepted fd is stranded and the listener stalls.
  private def acceptNext[K <: SocketKind](server: StreamServer[K]): EmIO[EmileError.Io, Socket[K]] =
    EffIO.lift(
      IO.uncancelable { poll =>
        poll(server.connections.take).flatMap:
          case Left(error) => IO.pure(Left(error))
          case Right(()) => Routing.onOwner(server.poller)(performAccept(server))
      }
    )

  private def chmodPipe(server: StreamServerState, mode: IpcMode): EmIO[EmileError.Io, Unit] =
    EffIO.lift(
      Routing.onOwner(server.poller):
        val rc = LibUV.uv_pipe_chmod(server.handle, modeFlag(mode))
        if rc < 0 then Left(IoMapping.fromCode(rc)) else Right(())
    )

  // uv_pipe_chmod reuses libuv's UV_READABLE / UV_WRITABLE flag values for the desired access.
  private def modeFlag(mode: IpcMode): Int = mode match
    case IpcMode.Readable => LibUV.UV_READABLE
    case IpcMode.Writable => LibUV.UV_WRITABLE
    case IpcMode.ReadWrite => LibUV.UV_READABLE | LibUV.UV_WRITABLE

  // FFI: client handle calloc with throw-on-OOM, uv_close cleanup of a half-built client.
  // scalafix:off DisableSyntax

  private def performAccept[K <: SocketKind](server: StreamServer[K]): Either[EmileError.Io, Socket[K]] =
    val acceptor = server.acceptor
    val client = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(acceptor.handleType))
    if client == null then throw new OutOfMemoryError("emile: client handle allocation failed")
    val initRc = acceptor.initClient(server.poller.loop, client)
    if initRc != 0 then
      stdlib.free(client)
      Left(IoMapping.fromCode(initRc))
    else
      val acceptRc = LibUV.uv_accept(server.handle, client)
      if acceptRc != 0 then
        LibUV.uv_close(client, freeHandleCb)
        Left(IoMapping.fromCode(acceptRc))
      else
        acceptor.captureAddresses(client) match
          case Left(error) =>
            LibUV.uv_close(client, freeHandleCb)
            Left(error)
          case Right((local, peer)) =>
            Right(Socket.construct[K](client, server.poller, local, peer))
    end if
  end performAccept

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end StreamServer
