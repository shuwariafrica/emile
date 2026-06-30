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
package emile.unsafe

import scala.scalanative.unsafe.Ptr

import cats.effect.IO

final private class LiveHandleState(val poller: LibUVPoller, val handle: Ptr[Byte]):
  // Owner-confined: markClosed writes this and tryUse reads it only inside Routing.onOwner, so every
  // access is on the one owner thread and needs no barrier. @volatile is kept solely as a cross-thread
  // visibility seam; it grants visibility, not mutual exclusion, so it cannot by itself make an
  // off-owner caller's pointer access safe - and no emile code reads it off the owner thread.
  @volatile var closed: Boolean = false // scalafix:ok DisableSyntax.var

/** A libuv handle paired with the liveness discipline that turns use-after-release into a typed
  * error instead of a native use-after-free: the raw freeable pointer is handed out only by
  * [[LiveHandle$ LiveHandle]].tryUse, and only while the handle is live, so a use-after-free is
  * unexpressible rather than merely discouraged. Operations live in [[LiveHandle$ LiveHandle]].
  */
private[emile] opaque type LiveHandle = LiveHandleState

/** Factory, owner-confined access guard, and reclamation for [[LiveHandle]] - the single audited
  * home of libuv handle freeing. Reclamation is concentrated here because a redundant double-close
  * corrupts the native heap: [[closeOnOwner]] is the one path that frees a handle, and it is
  * idempotent. Both [[tryUse]] and [[closeOnOwner]] run on the owner thread through
  * [[Routing.onOwner]], so a use cannot race the free.
  */
private[emile] object LiveHandle:

  /** Wrap an already-initialised libuv handle - called once acquisition has `calloc`'d the handle
    * and run its `uv_*_init`. The handle starts live; [[closeOnOwner]] is the only transition to
    * closed.
    */
  def apply(poller: LibUVPoller, handle: Ptr[Byte]): LiveHandle =
    new LiveHandleState(poller, handle)

  given CanEqual[LiveHandle, LiveHandle] = CanEqual.derived

  /** The owning loop's poller - the loop every operation on this handle must route to. */
  def poller(live: LiveHandle): LibUVPoller = live.poller

  /** Run `f` with the raw handle pointer while the handle is live, else yield `ifClosed`. Call only
    * on the owner thread (inside [[Routing.onOwner]]): that serialisation orders the liveness check
    * against [[closeOnOwner]], so `f` never touches a freed pointer.
    */
  inline def tryUse[A](live: LiveHandle, inline ifClosed: => A)(inline f: Ptr[Byte] => A): A =
    // by-name ifClosed: an effectful closed branch (e.g. failing a callback) must not run when live.
    if live.closed then ifClosed else f(live.handle)

  /** Reclaim the handle on its owner loop thread: mark it closed, then `uv_close` it and free its C
    * memory, completing once libuv's close callback has fired. Idempotent - a second call (or a
    * concurrent one) finds the flag set and is a no-op, so the handle is never double-closed.
    *
    * Generic reclamation only: any handle-type-specific quiescing (e.g. `uv_read_stop`) must
    * precede this so no live callback fires against the handle between the close request and the
    * free.
    */
  def closeOnOwner(live: LiveHandle): IO[Unit] =
    Routing.onOwner(live.poller)(markClosed(live)).flatMap {
      case true => Routing.closeHandle(live.poller, live.handle)
      case false => IO.unit
    }

  // Returns true only for the call that performs the transition, so exactly one closeOnOwner frees.
  private def markClosed(live: LiveHandle): Boolean =
    if live.closed then false
    else
      live.closed = true
      true

end LiveHandle
