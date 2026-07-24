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

import boilerplate.effect.given
import cats.effect.IO
import fs2.Stream

/** A set of replicated listeners, one per worker loop, sharing a single port through
  * `SO_REUSEPORT`; the kernel distributes incoming connections across them. Acquired through
  * [[TCP$ TCP]].bindPerWorker, and parameterised by its [[SocketKind]] exactly as [[StreamServer]]
  * is; accept and serve operations are on [[ServerPool$ ServerPool]].
  *
  * The kernel's distribution is even only asymptotically - skewed at low connection counts - so
  * [[serve]]'s global (not per-listener) limit, not an assumption of balanced per-listener load, is
  * how concurrency is bounded. Releasing the pool closes every listener; connections already
  * sitting in a listener's kernel accept queue but not yet accepted are reset by the kernel, not
  * redistributed, so stop directing traffic to the port before release where that matters.
  */
opaque type ServerPool[+K <: SocketKind] = List[StreamServer[K]]

/** A replicated pool of TCP listeners, acquired through [[TCP$ TCP]].bindPerWorker. */
type TCPServerPool = ServerPool[SocketKind.TCP]

/** Accept and serve operations, factory, and equality for [[ServerPool]]. Each operation composes
  * the per-listener [[StreamServer]] machinery across all N listeners: [[accepted]] merges their
  * accept streams, and [[serve]] runs them under one shared limit. Every accepted socket stays
  * pinned to the loop of the listener that accepted it, so per-socket operations route to the right
  * loop without a cross-loop hop.
  */
object ServerPool:

  given [K <: SocketKind] => CanEqual[ServerPool[K], ServerPool[K]] = CanEqual.derived

  extension [K <: SocketKind](pool: ServerPool[K])

    /** The listeners' accept streams merged into one: each element is a resource that accepts one
      * connection when used, yielding a `Socket[K]` already pinned to the loop of the listener that
      * accepted it. Safe under every combinator, as [[StreamServer.accepted]] is.
      */
    def accepted: EmStream[EmileError.IO, EmResource[EmileError.IO, Socket[K]]] =
      Stream.emits(pool).covary[EmIO.Of[EmileError.IO]].map(server => server.accepted).parJoinUnbounded

    /** Accepts across all listeners and runs `handler` on each connection, up to `maxConcurrent` at
      * a time GLOBALLY - the limit bounds the handlers running across every listener together, not
      * per listener - until `shutdown` completes. Resilience, the drain-not-cancel `shutdown`
      * contract, and `onError`'s union channel are exactly [[StreamServer.serve]]'s, extended over
      * the replicated set.
      */
    @targetName("ext_poolServe")
    def serve[E <: Throwable](maxConcurrent: Int, shutdown: IO[Unit])(handler: Socket[K] => EmIO[E, Unit])(
      onError: (EmileError.IO | E) => IO[Unit]
    )(using TypeTest[Throwable, E]): EmIO[Nothing, Unit] =
      StreamServer.serveAll(pool, maxConcurrent, shutdown, onError, handler)

    /** As the general [[serve]], for a handler that publishes no typed error of its own: everything
      * reaching `onError` is then emile's, a defect arriving as [[EmileError.IO.Unexpected]].
      */
    @targetName("ext_poolServeInfallible")
    def serve(maxConcurrent: Int, shutdown: IO[Unit])(handler: Socket[K] => EmIO[Nothing, Unit])(
      onError: EmileError.IO => IO[Unit]
    ): EmIO[Nothing, Unit] =
      StreamServer.serveAll[K, Nothing](pool, maxConcurrent, shutdown, onError, handler)

    /** The address each listener is bound to - all the same port, one entry per listener. */
    def addresses: List[AddressOf[K]] = pool.map(server => server.address)

    /** The replication factor achieved - the number of listeners, one per worker loop. */
    def size: Int = pool.length

  end extension

  /** Build a pool from its already-bound listeners - called once `TCP.bindPerWorker` has installed
    * one listener per worker loop.
    */
  private[emile] def construct[K <: SocketKind](listeners: List[StreamServer[K]]): ServerPool[K] = listeners

  /** Release every listener on its own loop - the pool's Resource finaliser. */
  private[emile] def release[K <: SocketKind](pool: ServerPool[K]): IO[Unit] =
    pool.foldLeft(IO.unit)((close, server) => close *> StreamServer.release(server))

end ServerPool
