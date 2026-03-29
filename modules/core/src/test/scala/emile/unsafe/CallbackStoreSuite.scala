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

import scala.scalanative.libc.stdlib.calloc
import scala.scalanative.libc.stdlib.free
import scala.scalanative.unsafe.*

import munit.FunSuite

import emile.HandleType
import emile.Loop

class CallbackStoreSuite extends FunSuite:
// scalafix:off

  /** Allocate a real libuv timer handle on the given loop. */
  private def initTimer(loopPtr: Ptr[Byte]): Ptr[Byte] =
    val size = LibUV.uv_handle_size(HandleType.toLibuvInline(HandleType.Timer))
    val handle = calloc(1L, size.toLong)
    assert(handle != null, "calloc for timer handle should succeed")
    val rc = LibUV.uv_timer_init(loopPtr, handle)
    assert(rc == 0, s"uv_timer_init failed with $rc")
    handle

  /** Close a handle synchronously (schedule close, run loop once). */
  private def closeHandle(loopPtr: Ptr[Byte], handle: Ptr[Byte]): Unit =
    LibUV.uv_close(handle, null.asInstanceOf[LibUV.CloseCB])
    val _ = LibUV.uv_run(loopPtr, 0) // RunMode.Default

  test("attach and get round-trip"):
    val loop = Loop.create.toOption.get
    val handle = initTimer(loop.ptrUnsafe)

    val cb: () => Unit = () => ()
    CallbackStore.attach(handle, cb)
    val retrieved = CallbackStore.get[() => Unit](handle)

    assert(retrieved.isDefined, "get should return the attached callback")
    assert(retrieved.get eq cb, "get should return the exact same callback object")

    CallbackStore.detach(handle)
    closeHandle(loop.ptrUnsafe, handle)
    val _ = loop.close

  test("get returns None after detach"):
    val loop = Loop.create.toOption.get
    val handle = initTimer(loop.ptrUnsafe)

    val cb: () => Unit = () => ()
    CallbackStore.attach(handle, cb)
    CallbackStore.detach(handle)

    val retrieved = CallbackStore.get[() => Unit](handle)
    assert(retrieved.isEmpty, "get should return None after detach")

    closeHandle(loop.ptrUnsafe, handle)
    val _ = loop.close

  test("attach replaces previous callback"):
    val loop = Loop.create.toOption.get
    val handle = initTimer(loop.ptrUnsafe)

    val cb1: () => Unit = () => ()
    val cb2: () => Unit = () => ()
    CallbackStore.attach(handle, cb1)
    CallbackStore.attach(handle, cb2)

    val retrieved = CallbackStore.get[() => Unit](handle)
    assert(retrieved.isDefined, "get should return the replacement callback")
    assert(retrieved.get eq cb2, "get should return the second callback, not the first")
    assert(!(retrieved.get eq cb1), "first callback should have been replaced")

    CallbackStore.detach(handle)
    closeHandle(loop.ptrUnsafe, handle)
    val _ = loop.close

  test("attachReq and detachReq round-trip"):
    val loop = Loop.create.toOption.get
    // Allocate a request (use getaddrinfo as a reasonably-sized request type)
    val reqSize = LibUV.uv_req_size(6) // UV_GETADDRINFO = 6
    val req = calloc(1L, reqSize.toLong)
    assert(req != null, "calloc for request should succeed")

    val cb: () => Unit = () => ()
    CallbackStore.attachReq(req, cb)

    val retrieved = CallbackStore.detachReq[() => Unit](req)
    assert(retrieved.isDefined, "detachReq should return the attached callback")
    assert(retrieved.get eq cb, "detachReq should return the exact same callback object")

    // After detachReq, a second detachReq should return None
    val again = CallbackStore.detachReq[() => Unit](req)
    assert(again.isEmpty, "second detachReq should return None")

    free(req)
    val _ = loop.close

  test("detach on handle with no callback is a no-op"):
    val loop = Loop.create.toOption.get
    val handle = initTimer(loop.ptrUnsafe)

    // Should not throw or crash
    CallbackStore.detach(handle)
    val retrieved = CallbackStore.get[() => Unit](handle)
    assert(retrieved.isEmpty, "get on never-attached handle should return None")

    closeHandle(loop.ptrUnsafe, handle)
    val _ = loop.close

end CallbackStoreSuite
