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
package emile

import scala.scalanative.libc.stdlib.free
import scala.scalanative.unsafe.*

import emile.unsafe.CallbackStore
import emile.unsafe.LibUV

/** Type class for libuv handle operations.
  *
  * All handle types share these base operations for lifecycle management, reference counting, and
  * loop access.
  *
  * @tparam H The handle type
  */
trait Handle[H]:
  extension (h: H)
    /** Close the handle asynchronously.
      *
      * This is the safe way to dispose of a handle. The callback will be invoked when the close is
      * complete.
      */
    def close(callback: Either[EmileError, Unit] => Unit): Unit

    /** Close the handle without callback.
      *
      * Note: The close is still asynchronous, but no notification is provided.
      */
    def close: Either[EmileError, Unit]

    /** Close the handle synchronously (no callback) with consistent semantics. */
    def closeSync: Either[EmileError, Unit]

    /** Check if handle is active.
      *
      * What "active" means depends on the handle type:
      *   - TCP: listening, connected, or reading
      *   - Timer: started and not stopped
      *   - Async: always active
      */
    def isActive: Boolean

    /** Check if handle is closing or closed.
      *
      * Once a handle is closing, no operations should be performed on it.
      */
    def isClosing: Boolean

    /** Reference the handle.
      *
      * Referenced handles keep the event loop alive. By default, handles are referenced when
      * created.
      */
    def ref: Unit

    /** Unreference the handle.
      *
      * Unreferenced handles do not keep the event loop alive. Useful for long-running background
      * handles.
      */
    def unref: Unit

    /** Check if handle is referenced.
      *
      * @return true if handle is referenced
      */
    def hasRef: Boolean

    /** Get the owning event loop.
      *
      * @return The loop this handle belongs to
      */
    def loop: Loop

    /** Get handle type.
      *
      * @return The type of this handle
      */
    def handleType: HandleType

    /** Get the raw pointer for advanced usage. */
    def ptrUnsafe: Ptr[Byte]
  end extension
end Handle

object Handle:
  /** Summon Handle instance for type H.
    *
    * Uses `transparent inline` to preserve the specific instance type, enabling better type
    * inference at call sites.
    */
  transparent inline def apply[H](using h: Handle[H]): Handle[H] = h

  /** Create a Handle instance from a raw pointer type.
    *
    * This provides the common implementation for all handle types.
    */
  private[emile] def fromPtr[H](toPtr: H => Ptr[Byte]): Handle[H] = new Handle[H]:
    extension (h: H)
      def close(callback: Either[EmileError, Unit] => Unit): Unit =
        val ptr = toPtr(h)
        if LibUV.uv_is_closing(ptr) != 0 then callback(Left(EmileError.AlreadyClosed))
        else
          CallbackStore.attach(ptr, callback)
          LibUV.uv_close(ptr, closeCallback)

      def close: Either[EmileError, Unit] =
        closeSync

      def closeSync: Either[EmileError, Unit] =
        val ptr = toPtr(h)
        if LibUV.uv_is_closing(ptr) != 0 then Left(EmileError.AlreadyClosed)
        else
          CallbackStore.detach(ptr)
          LibUV.uv_close(ptr, nullCloseCallback)
          Right(())

      def isActive: Boolean =
        LibUV.uv_is_active(toPtr(h)) != 0

      def isClosing: Boolean =
        LibUV.uv_is_closing(toPtr(h)) != 0

      def ref: Unit =
        LibUV.uv_ref(toPtr(h))

      def unref: Unit =
        LibUV.uv_unref(toPtr(h))

      def hasRef: Boolean =
        LibUV.uv_has_ref(toPtr(h)) != 0

      def loop: Loop =
        LibUV.uv_handle_get_loop(toPtr(h)).asInstanceOf[Loop] // scalafix:ok; libuv returns opaque Ptr

      def handleType: HandleType =
        HandleType.fromLibuv(LibUV.uv_handle_get_type(toPtr(h)))

      def ptrUnsafe: Ptr[Byte] = toPtr(h)
    end extension

  /** Close a handle asynchronously, detaching any existing data-field callback first.
    *
    * This is the standard close pattern for handles that store operational callbacks (timers,
    * signals, etc.) in the data field. It detaches the current callback, attaches the close
    * callback, then calls uv_close.
    */
  private[emile] def closeAsyncWithDetach(ptr: Ptr[Byte], callback: Either[EmileError, Unit] => Unit): Unit =
    if LibUV.uv_is_closing(ptr) != 0 then callback(Left(EmileError.AlreadyClosed))
    else
      CallbackStore.detach(ptr)
      CallbackStore.attach(ptr, callback)
      LibUV.uv_close(ptr, closeCallback)

  /** Close callback: invokes stored Scala callback, then frees handle memory. */
  private[emile] val closeCallback: LibUV.CloseCB = (handle: Ptr[Byte]) =>
    CallbackStore.get[Either[EmileError, Unit] => Unit](handle).foreach { callback =>
      CallbackStore.detach(handle)
      callback(Right(()))
    }
    free(handle)

  /** Close callback: detaches any callback, then frees handle memory. */
  private[emile] val nullCloseCallback: LibUV.CloseCB = (handle: Ptr[Byte]) =>
    CallbackStore.detach(handle)
    free(handle)
end Handle
