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

import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.runtime.toRawPtr
import scala.scalanative.unsafe.Ptr
import scala.util.control.NonFatal

import cats.effect.IO

import emile.EmileError

/** Worker-affinity routing: runs a thunk on the libuv loop thread that owns a [[LibUVPoller]], with
  * a cancellation finaliser on the cross-thread path.
  */
private[emile] object Routing:

  /** Run `thunk` on the thread that owns `poller`'s loop.
    *
    * The ownership check and the fast-path run of `thunk` occupy a single `IO.delay` node by
    * design: cats-effect can auto-cede between nodes and a stealing worker would otherwise run
    * `thunk` - an off-thread libuv call, i.e. a native data race on the loop. A rescheduled node
    * re-runs and re-checks ownership wholesale, so the guarantee survives work-stealing. Off the
    * owner the node yields `Left` and the call is submitted to the loop thread, with a finaliser
    * that removes the still-queued runnable on cancellation.
    */
  def onOwner[A](poller: LibUVPoller)(thunk: => A): IO[A] =
    IO.delay {
      if poller.isOwnerThread then Right(runOnOwner(thunk))
      else Left(())
    }.flatMap {
      case Right(outcome) => IO.fromEither(outcome)
      case Left(()) =>
        IO.async[A]: cb =>
          IO.delay:
            val runnable: Runnable = () => cb(runOffOwner(thunk))
            if poller.submit(runnable) then Some(IO.delay { poller.remove(runnable): Unit; () })
            else
              cb(Left(EmileError.IO.AlreadyClosed))
              None
    }

  // Fast path on the owner: only NonFatal is captured; a fatal throw propagates to cats-effect's
  // fatal handler.
  private def runOnOwner[A](thunk: => A): Either[Throwable, A] =
    try Right(thunk)
    catch case NonFatal(t) => Left(t)

  // Cross-thread path from the submitted runnable: captures every throwable so the IO.async callback
  // always fires - a fatal escaping here is swallowed by the poller's taskDrainCb, hanging the fibre
  // on a callback that never completes.
  private def runOffOwner[A](thunk: => A): Either[Throwable, A] =
    try Right(thunk)
    catch case t: Throwable => Left(t)

  /** Close `handle` on `poller`'s loop thread, completing once libuv's close callback has fired and
    * freed the handle's C memory - the canonical release step for a libuv handle. The completion
    * callback rides through a `(LibUVPoller, cb)` closure in the handle's `data` slot so the close
    * callback can release the anchor entry before freeing the C memory.
    */
  def closeHandle(poller: LibUVPoller, handle: Ptr[Byte]): IO[Unit] =
    IO.async[Unit]: cb =>
      onOwner(poller):
        CallbackBridge.store(poller, handle, new CloseCompletion(poller, cb))
        LibUV.uv_close(handle, closeHandleCb)
        None

  // Holder for closeHandle's completion: carries the poller (for anchor release) and the continuation.
  final private[unsafe] class CloseCompletion(val poller: LibUVPoller, val cb: Either[Throwable, Unit] => Unit)

  // uv_close callback for closeHandle: release the anchor, free the handle, then complete the release.
  private val closeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) =>
    val completion = CallbackBridge.load[CloseCompletion](handle)
    CallbackBridge.clear(completion.poller, handle)
    stdlib.free(handle)
    completion.cb(Right(()))

end Routing

/** Storage of a callback holder in a libuv handle's or request's `data` slot, paired with a
  * GC-reachability anchor in the owning [[LibUVPoller.anchors]] map.
  * `Intrinsics.castObjectToRawPtr` is a pure reinterpretation, so without the anchor a stored
  * holder is collectible the moment its last Scala-side reference goes out of scope - cf.
  * cats-effect `EpollSystem`'s `handles` map. [[clear]] / [[releaseReq]] remove the anchor when the
  * slot is released; the trampoline-side [[load]] / [[loadReq]] recover the holder via the raw
  * pointer.
  */
private[emile] object CallbackBridge:

  /** Store `holder` in `handle`'s `uv_handle->data` slot and anchor it in `poller.anchors`. */
  inline def store(poller: LibUVPoller, handle: Ptr[Byte], holder: AnyRef): Unit =
    poller.anchors.put(addrOf(handle), holder): Unit
    LibUV.uv_handle_set_data(handle, fromRawPtr[Byte](Intrinsics.castObjectToRawPtr(holder)))

  /** Recover the holder previously [[store]]d in `handle`'s `data` slot. */
  inline def load[H <: AnyRef](handle: Ptr[Byte]): H =
    // FFI recovery: the slot holds exactly the H that `store` placed there.
    Intrinsics.castRawPtrToObject(toRawPtr(LibUV.uv_handle_get_data(handle))).asInstanceOf[H] // scalafix:ok

  /** Clear `handle`'s `data` slot and release its anchor entry. */
  inline def clear(poller: LibUVPoller, handle: Ptr[Byte]): Unit =
    poller.anchors.remove(addrOf(handle)): Unit
    LibUV.uv_handle_set_data(handle, fromRawPtr[Byte](Intrinsics.castLongToRawPtr(0L)))

  /** Store `holder` in `req`'s `uv_req->data` slot, anchor it in `poller.anchors`, and record the
    * request in `poller.outstandingReqs` so `close()` can cancel it.
    */
  inline def storeReq(poller: LibUVPoller, req: Ptr[Byte], holder: AnyRef): Unit =
    poller.anchors.put(addrOf(req), holder): Unit
    poller.outstandingReqs.put(addrOf(req), req): Unit
    LibUV.uv_req_set_data(req, fromRawPtr[Byte](Intrinsics.castObjectToRawPtr(holder)))

  /** Release a request's anchor and outstanding-request record - paired one-to-one with
    * [[storeReq]]; the request's `data` slot is freed alongside the request itself, so no slot
    * null-out is needed.
    */
  inline def releaseReq(poller: LibUVPoller, req: Ptr[Byte]): Unit =
    poller.anchors.remove(addrOf(req)): Unit
    poller.outstandingReqs.remove(addrOf(req)): Unit

  /** Recover the holder previously [[storeReq]]d in `req`'s `data` slot. */
  inline def loadReq[H <: AnyRef](req: Ptr[Byte]): H =
    // FFI recovery: the slot holds exactly the H that `storeReq` placed there.
    Intrinsics.castRawPtrToObject(toRawPtr(LibUV.uv_req_get_data(req))).asInstanceOf[H] // scalafix:ok

  private inline def addrOf(ptr: Ptr[Byte]): Long =
    Intrinsics.castRawPtrToLong(toRawPtr(ptr))

end CallbackBridge
