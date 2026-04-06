/*
 * Copyright 2025, 2026 Ali Rashid.
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

// scalafix:off DisableSyntax.null, DisableSyntax.asInstanceOf; unsafe FFI callback storage

import scala.annotation.internal.sharable
import scala.scalanative.runtime.Intrinsics.castObjectToRawPtr
import scala.scalanative.runtime.Intrinsics.castRawPtrToObject
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.runtime.toRawPtr
import scala.scalanative.unsafe.*

/** Zero-indirection callback storage for libuv handles and requests.
  *
  * Callbacks are stored as raw object pointers in the native data fields (`uv_handle_set_data` /
  * `uv_req_set_data`) and recovered via `castRawPtrToObject` — O(1) with no map lookup.
  *
  * ==GC Safety==
  *
  * The garbage collector does not scan libuv's native memory. A callback stored only as a raw
  * pointer in a handle's data field would be collected. The `gcRoots` set holds strong Scala-side
  * references to all registered callbacks, preventing collection while they are reachable from
  * native code.
  *
  * This is sound for Scala Native's current non-moving GC (Immix/Commix). If a future GC introduces
  * object relocation, Scala Native will need to provide a pinning mechanism for FFI — at which
  * point all native interop (not just this module) would require adaptation.
  *
  * ==Thread Safety==
  *
  * The `gcRoots` set uses `ConcurrentHashMap` and is safe for concurrent access. Handle/request
  * data fields are accessed only from the loop's owning thread (a libuv invariant).
  */
@sharable
private[emile] object CallbackStore:
  private inline def uv = _root_.emile.unsafe.LibUV

  /** Strong references preventing GC of callbacks stored in native data fields.
    *
    * Uses java.util.IdentityHashMap for O(1) identity-based lookup (no hashCode/equals calls on
    * stored objects). Wrapped in Collections.synchronizedMap for thread safety. Using identity
    * semantics avoids calling any methods on the stored callbacks.
    */
  private val gcRoots: java.util.Map[AnyRef, java.lang.Boolean] =
    java.util.Collections.synchronizedMap(new java.util.IdentityHashMap[AnyRef, java.lang.Boolean]())

  // =========================================================================
  // Handle callbacks
  // =========================================================================

  /** Attach a callback to a handle's native data field.
    *
    * Any previously attached callback is detached first (preventing leaks when a handle's callback
    * is replaced, e.g., signal start → close).
    *
    * @param handle Raw handle pointer
    * @param callback The Scala callback object
    */
  def attach(handle: Ptr[Byte], callback: AnyRef): Unit =
    detach(handle) // unpin previous, if any
    gcRoots.put(callback, java.lang.Boolean.TRUE)
    uv.uv_handle_set_data(handle, toPtr(callback))

  /** Retrieve the callback from a handle's native data field.
    *
    * O(1) — single pointer dereference and cast, no map lookup.
    */
  def get[A](handle: Ptr[Byte]): Option[A] =
    val data = uv.uv_handle_get_data(handle)
    if data == null then None
    else Some(fromPtr[A](data))

  /** Detach the callback from a handle and release the GC root. */
  def detach(handle: Ptr[Byte]): Unit =
    val data = uv.uv_handle_get_data(handle)
    if data != null then
      val _ = gcRoots.remove(fromPtr[AnyRef](data))
      uv.uv_handle_set_data(handle, null.asInstanceOf[Ptr[Byte]])

  // =========================================================================
  // Request callbacks
  // =========================================================================

  /** Attach a callback to a request's native data field. */
  def attachReq(req: Ptr[Byte], callback: AnyRef): Unit =
    gcRoots.put(callback, java.lang.Boolean.TRUE)
    uv.uv_req_set_data(req, toPtr(callback))

  /** Retrieve and detach the callback from a request.
    *
    * Requests are single-use, so retrieval always detaches.
    */
  def detachReq[A](req: Ptr[Byte]): Option[A] =
    val data = uv.uv_req_get_data(req)
    if data == null then None
    else
      val cb = fromPtr[A](data)
      val _ = gcRoots.remove(cb.asInstanceOf[AnyRef])
      uv.uv_req_set_data(req, null.asInstanceOf[Ptr[Byte]])
      Some(cb)

  // =========================================================================
  // Pointer conversions (zero-cost inline)
  // =========================================================================

  private inline def toPtr(obj: AnyRef): Ptr[Byte] =
    fromRawPtr[Byte](castObjectToRawPtr(obj))

  private inline def fromPtr[A](ptr: Ptr[Byte]): A =
    castRawPtrToObject(toRawPtr(ptr)).asInstanceOf[A]

end CallbackStore
