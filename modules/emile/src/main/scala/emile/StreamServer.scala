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
import scala.reflect.TypeTest
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import boilerplate.effect.given
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Semaphore
import cats.effect.std.Supervisor
import cats.effect.std.unsafe.UnboundedQueue
import fs2.Stream

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.Routing

final private[emile] class StreamServerState(
  val live: LiveHandle,
  val address: Matchable,
  val connections: UnboundedQueue[IO, Either[EmileError.IO, Unit]],
  val acceptor: StreamServer.Acceptor
)

/** A listening stream server, parameterised by its [[SocketKind]] - the shared accept surface of
  * TCP and [[IPC$ IPC]] (Unix-domain / named-pipe) listeners. Covariant in `K`, so a [[TCPServer]]
  * is usable as an [[AnyServer]]. Accept operations are on [[StreamServer$ StreamServer]].
  */
opaque type StreamServer[+K <: SocketKind] = StreamServerState

/** A listening TCP server, acquired through [[TCP$ TCP]]. */
type TCPServer = StreamServer[SocketKind.TCP]

/** A listening [[IPC$ IPC]] (Unix-domain / named-pipe) server, acquired through [[IPC$ IPC]]. */
type IPCServer = StreamServer[SocketKind.IPC]

/** A stream server of any kind - the neutral spelling over which the shared operations resolve. */
type AnyServer = StreamServer[SocketKind]

/** Accept operations, the per-kind accept strategy, factories, and equality for [[StreamServer]].
  * The accept machinery is defined once over `StreamServer[K]` and shared by every kind; the
  * kind-specific steps (client-handle init, address capture, post-accept finish) are carried by the
  * [[Acceptor]] each transport builds at bind. Accepting on a `StreamServer[K]` yields a
  * `Socket[K]`, so the right kind-specific socket operations resolve.
  *
  * As with [[Socket$ Socket]], every operation reaches the raw handle through
  * [[emile.unsafe.LiveHandle LiveHandle]], so an `accept` or `chmod` after the server's resource
  * has released is a typed [[EmileError.IO.AlreadyClosed]], not a use-after-free.
  */
object StreamServer:

  given [K <: SocketKind] => CanEqual[StreamServer[K], StreamServer[K]] = CanEqual.derived

  /** The per-kind accept strategy a [[StreamServer]] is built with - how to allocate, initialise,
    * and address a client handle, and the post-accept `finish` step (the TCP tuning; nothing for
    * IPC). Built once at bind by the transport entry ([[TCP$ TCP]] / [[IPC$ IPC]]) and carried in
    * the server state, so the shared accept machinery stays kind-agnostic.
    */
  final private[emile] class Acceptor(
    val handleType: Int,
    val initClient: (Ptr[Byte], Ptr[Byte]) => CInt,
    val captureAddresses: Ptr[Byte] => Either[EmileError.IO, (Matchable, Matchable)],
    val finish: AnySocket => EmIO[EmileError.IO, Unit]
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
    def accepted: EmStream[EmileError.IO, EmResource[EmileError.IO, Socket[K]]] =
      Stream.emit(server.acceptOne).covary[EmIO.Of[EmileError.IO]].repeat

    /** A single accepted connection as a scoped resource - the socket closes when the resource's
      * scope ends. The kind's post-accept finish step (TCP tuning; nothing for IPC) runs before the
      * socket is yielded.
      */
    def acceptOne: EmResource[EmileError.IO, Socket[K]] =
      // makeFull keeps the accept wait cancelable: Resource.make's acquire is uncancelable, which would
      // mask acceptNext's poll(take) and hang a cancelled idle accept (e.g. a graceful shutdown).
      Resource
        .makeFull[EffIO.Of[EmileError.IO], Socket[K]](poll => poll(acceptNext(server)))(socket => EffIO.liftF(Socket.release(socket)))
        .evalTap(socket => server.acceptor.finish(socket))

    /** Accepts connections and runs `handler` on each, up to `maxConcurrent` at a time, until
      * `shutdown` completes; the socket is released when its handler returns. The server is
      * resilient - one connection never brings down the rest: a failing handler, a failed accept,
      * or a handler defect goes to `onError` (a defect as `EmileError.IO.Unexpected`) and the loop
      * carries on. `shutdown` drains rather than cancels, letting in-flight handlers finish before
      * the effect returns, so a handler that never returns holds shutdown open. Compose
      * [[accepted]] directly for a server that stops on the first failure.
      */
    @targetName("ext_serve")
    def serve[E <: Throwable](maxConcurrent: Int, shutdown: IO[Unit])(handler: Socket[K] => EmIO[E, Unit])(
      onError: (EmileError.IO | E) => IO[Unit]
    )(using TypeTest[Throwable, E]): EmIO[Nothing, Unit] =
      serveLoop(server, maxConcurrent, shutdown, onError, handler)

    /** As the general [[serve]], for a handler that publishes no typed error of its own: everything
      * reaching `onError` is then emile's, a defect arriving as [[EmileError.IO.Unexpected]].
      */
    @targetName("ext_serveInfallible")
    def serve(maxConcurrent: Int, shutdown: IO[Unit])(handler: Socket[K] => EmIO[Nothing, Unit])(
      onError: EmileError.IO => IO[Unit]
    ): EmIO[Nothing, Unit] =
      // Pinning E = Nothing on a more specific handler keeps the solver from widening it to Throwable,
      // whose type test is the identity - every defect would then reach onError raw, unwrapped.
      serveLoop[K, Nothing](server, maxConcurrent, shutdown, onError, handler)

  end extension

  extension (server: IPCServer)

    /** Set the socket file's access mode via `uv_pipe_chmod`. A filesystem-path server only - an
      * abstract-namespace server has no socket file and yields a libuv error.
      */
    @targetName("ext_chmod")
    inline def chmod(mode: IPCMode): EmIO[EmileError.IO, Unit] =
      chmodPipe(server, mode)

  /** Build the server state of kind `K` and store it in the listen handle's `data` slot so
    * [[connectionCb]] can recover it. Storing here, inside the companion, avoids an opaque-type
    * cast at the call site.
    */
  private[emile] def construct[K <: SocketKind](
    handle: Ptr[Byte],
    poller: LibUVPoller,
    address: Matchable,
    connections: UnboundedQueue[IO, Either[EmileError.IO, Unit]],
    acceptor: Acceptor
  ): StreamServer[K] =
    val state = new StreamServerState(LiveHandle(poller, handle), address, connections, acceptor)
    CallbackBridge.store(poller, handle, state)
    state

  /** Release the listener through [[emile.unsafe.LiveHandle LiveHandle]]: mark it closed - so a
    * racing `accept` / `chmod` yields a typed [[EmileError.IO.AlreadyClosed]] rather than touching
    * a freed handle - then `uv_close` it. The close stores its own completion over the handle's
    * `data` slot and runs in one owner-thread step, which stops any further `connection_cb`; a
    * separate pre-clear would instead open a window where a `connection_cb` reads a nulled slot and
    * dereferences it across the C ABI.
    */
  private[emile] def release(server: AnyServer): IO[Unit] =
    LiveHandle.closeOnOwner(server.live)

  /** `uv_connection_cb`, on the server's loop thread: offers a connection signal to the accept
    * queue. libuv on Linux load-sheds accept failures itself and only ever calls this with status
    * 0; the negative-status branch carries the accept errors other platforms deliver here.
    */
  private[emile] val connectionCb: LibUV.ConnectionCB = (handle: Ptr[Byte], status: CInt) =>
    val state = CallbackBridge.load[StreamServerState](handle)
    val signal: Either[EmileError.IO, Unit] = if status < 0 then Left(IOMapping.fromCode(status)) else Right(())
    state.connections.unsafeOffer(signal)

  // The wait is cancelable, but the take -> uv_accept transition is not: a cancel that has consumed a
  // connection signal must still run uv_accept, or the accepted fd is stranded and the listener stalls.
  private def acceptNext[K <: SocketKind](server: StreamServer[K]): EmIO[EmileError.IO, Socket[K]] =
    EffIO.lift(
      IO.uncancelable { poll =>
        poll(server.connections.take).flatMap:
          case Left(error) => IO.pure(Left(error))
          case Right(()) => Routing.onOwner(poller(server))(performAccept(server))
      }
    )

  private def chmodPipe(server: StreamServerState, mode: IPCMode): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(server)):
        LiveHandle.tryUse(server.live, closedIOUnit): handle =>
          val rc = chmodHandle(handle, mode)
          if rc < 0 then Left(IOMapping.fromCode(rc)) else Right(())
    )

  /** Set a raw pipe handle's socket-file access mode via `uv_pipe_chmod`, returning the libuv rc -
    * shared by the public [[chmod]] and [[IPC$ IPC]].bind's in-acquire hardening. Call on the owner
    * thread.
    */
  private[emile] def chmodHandle(handle: Ptr[Byte], mode: IPCMode): Int =
    LibUV.uv_pipe_chmod(handle, modeFlag(mode))

  // The owning loop's poller - stored in the LiveHandle beside the handle, valid until release.
  private def poller(server: StreamServerState): LibUVPoller = LiveHandle.poller(server.live)

  // Closed-branch results for a LiveHandle guard whose live branch returns an Either.
  private val closedIOUnit: Either[EmileError.IO, Unit] = Left(EmileError.IO.AlreadyClosed)
  private def closedAccept[K <: SocketKind]: Either[EmileError.IO, Socket[K]] = Left(EmileError.IO.AlreadyClosed)

  // uv_pipe_chmod reuses libuv's UV_READABLE / UV_WRITABLE flag values for the desired access.
  private def modeFlag(mode: IPCMode): Int = mode match
    case IPCMode.Readable => LibUV.UV_READABLE
    case IPCMode.Writable => LibUV.UV_WRITABLE
    case IPCMode.ReadWrite => LibUV.UV_READABLE | LibUV.UV_WRITABLE

  private def serveLoop[K <: SocketKind, E <: Throwable](
    server: StreamServer[K],
    maxConcurrent: Int,
    shutdown: IO[Unit],
    onError: (EmileError.IO | E) => IO[Unit],
    handler: Socket[K] => EmIO[E, Unit]
  )(using TypeTest[Throwable, E]): EmIO[Nothing, Unit] =
    serveScaffold(maxConcurrent)((permits, supervisor) => acceptLoop(server, shutdown, permits, supervisor, onError, handler))

  /** Run the accept-and-handle loop over every server in `servers` under ONE `Supervisor` and ONE
    * `Semaphore`, so `maxConcurrent` bounds the handlers running across all of them together - the
    * global-limit path a replicated [[ServerPool$ ServerPool]] serves on. Each server's loop runs
    * on its own poller, so an accepted socket stays pinned to the listener that accepted it.
    */
  private[emile] def serveAll[K <: SocketKind, E <: Throwable](
    servers: List[StreamServer[K]],
    maxConcurrent: Int,
    shutdown: IO[Unit],
    onError: (EmileError.IO | E) => IO[Unit],
    handler: Socket[K] => EmIO[E, Unit]
  )(using TypeTest[Throwable, E]): EmIO[Nothing, Unit] =
    serveScaffold(maxConcurrent)((permits, supervisor) =>
      Stream
        .emits(servers)
        .covary[IO]
        .parEvalMapUnordered(servers.length.max(1))(server => acceptLoop(server, shutdown, permits, supervisor, onError, handler))
        .compile
        .drain
    )

  // An awaiting supervisor gives serve its shutdown semantics: a normal exit (shutdown fired) joins the
  // in-flight handlers - the drain-not-cancel contract - while a cancellation cancels them. maxConcurrent
  // < 1 is a programmer error, hence a defect rather than a typed failure. `run` receives the shared
  // permits and supervisor so a single listener and a replicated pool share one global limit.
  private def serveScaffold(maxConcurrent: Int)(run: (Semaphore[IO], Supervisor[IO]) => IO[Unit]): EmIO[Nothing, Unit] =
    EffIO.liftF(
      IO.raiseWhen(maxConcurrent < 1)(new IllegalArgumentException("serve maxConcurrent must be at least 1")) *>
        Supervisor[IO](await = true).use(supervisor => Semaphore[IO](maxConcurrent.toLong).flatMap(permits => run(permits, supervisor)))
    )

  private def acceptLoop[K <: SocketKind, E <: Throwable](
    server: StreamServer[K],
    shutdown: IO[Unit],
    permits: Semaphore[IO],
    supervisor: Supervisor[IO],
    onError: (EmileError.IO | E) => IO[Unit],
    handler: Socket[K] => EmIO[E, Unit]
  )(using TypeTest[Throwable, E]): IO[Unit] =
    acceptAndFork(server, shutdown, permits, supervisor, onError, handler).flatMap(continue =>
      if continue then acceptLoop(server, shutdown, permits, supervisor, onError, handler) else IO.unit
    )

  // Wait for a connection first, THEN acquire a permit: the permit bounds concurrent handlers, not the
  // wait, so a pool of N listeners sharing one Semaphore never starves - every listener can take its own
  // connection even when maxConcurrent < N (permit-before-wait would let only maxConcurrent listeners ever
  // accept). The kernel accept queue still absorbs backpressure while a permit is awaited: the connection
  // is not uv_accept'd until the permit is held. The accept and the handler-fork are one uncancelable step,
  // so an accepted socket is always owned by a supervised fiber that releases it - a cancel can never
  // strand one. Both waits are cancelable and neither holds a socket or a permit.
  private def acceptAndFork[K <: SocketKind, E <: Throwable](
    server: StreamServer[K],
    shutdown: IO[Unit],
    permits: Semaphore[IO],
    supervisor: Supervisor[IO],
    onError: (EmileError.IO | E) => IO[Unit],
    handler: Socket[K] => EmIO[E, Unit]
  )(using TypeTest[Throwable, E]): IO[Boolean] =
    IO.uncancelable { poll =>
      poll(IO.race(shutdown, server.connections.take)).flatMap:
        case Left(()) => IO.pure(false)
        case Right(Left(error)) => onError(error).as(true)
        case Right(Right(())) =>
          poll(IO.race(shutdown, permits.acquire)).flatMap:
            case Left(()) => IO.pure(false)
            case Right(()) =>
              Routing
                .onOwner(poller(server))(performAccept(server))
                .flatMap:
                  case Left(error) => permits.release *> onError(error).as(true)
                  case Right(socket) =>
                    supervisor
                      .supervise(serveConnection(server, socket, onError, handler).guarantee(Socket.release(socket) *> permits.release))
                      .as(true)
    }

  // Routes a finish/handler typed failure (E) or a handler defect (EmileError.IO) to onError, so one
  // connection never brings the server down. The socket is released by acceptAndFork's guarantee, not here.
  private def serveConnection[K <: SocketKind, E <: Throwable](
    server: StreamServer[K],
    socket: Socket[K],
    onError: (EmileError.IO | E) => IO[Unit],
    handler: Socket[K] => EmIO[E, Unit]
  )(using TypeTest[Throwable, E]): IO[Unit] =
    server.acceptor
      .finish(socket)
      .either
      .flatMap:
        case Left(error) => onError(error)
        case Right(()) =>
          handler(socket).either.attempt.flatMap:
            case Right(Right(())) => IO.unit
            case Right(Left(error)) => onError(error)
            case Left(defect) => onError(EmileError.IO.Unexpected(defect))

  // FFI: client handle calloc with throw-on-OOM, uv_close cleanup of a half-built client.
  // scalafix:off DisableSyntax

  private def performAccept[K <: SocketKind](server: StreamServer[K]): Either[EmileError.IO, Socket[K]] =
    val result = performAcceptRaw(server)
    poller(server).metrics.acceptSettled(result.isRight)
    result

  // On the owner thread, so the LiveHandle guard makes an accept after the server released a typed
  // AlreadyClosed rather than a use-after-free.
  private def performAcceptRaw[K <: SocketKind](server: StreamServer[K]): Either[EmileError.IO, Socket[K]] =
    LiveHandle.tryUse(server.live, closedAccept[K]): serverHandle =>
      val acceptor = server.acceptor
      val client = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(acceptor.handleType))
      if client == null then throw new OutOfMemoryError("emile: client handle allocation failed")
      val initRc = acceptor.initClient(poller(server).loop, client)
      if initRc != 0 then
        stdlib.free(client)
        Left(IOMapping.fromCode(initRc))
      else
        val acceptRc = LibUV.uv_accept(serverHandle, client)
        if acceptRc != 0 then
          LibUV.uv_close(client, freeHandleCb)
          Left(IOMapping.fromCode(acceptRc))
        else
          acceptor.captureAddresses(client) match
            case Left(error) =>
              LibUV.uv_close(client, freeHandleCb)
              Left(error)
            case Right((local, peer)) =>
              Right(Socket.construct[K](client, poller(server), local, peer))
      end if
  end performAcceptRaw

  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

  // scalafix:on DisableSyntax

end StreamServer
