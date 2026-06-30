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
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*

import emile.LoopConfig

/** Covers [[CallbackBridge]]: a holder stored in a libuv handle's data slot round-trips through
  * `load`, a later `store` overwrites it, and the per-poller anchor map keeps the holder
  * GC-reachable (verified indirectly by the `clear` removing the entry).
  */
final class CallbackBridgeSpec extends munit.FunSuite:

  // A handle-sized zeroed buffer; uv_handle_set_data / uv_handle_get_data touch only the data slot.
  private def withPollerAndHandle[A](body: (LibUVPoller, Ptr[Byte]) => A): A =
    val poller = new LibUVPoller(LoopConfig.default)
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_ASYNC))
    try body(poller, handle)
    finally
      stdlib.free(handle)
      poller.close()

  test("store then load round-trips the holder") {
    withPollerAndHandle { (poller, handle) =>
      val holder: AnyRef = new Object
      CallbackBridge.store(poller, handle, holder)
      assert(CallbackBridge.load[AnyRef](handle) eq holder)
      CallbackBridge.clear(poller, handle)
    }
  }

  test("a second store overwrites the holder") {
    withPollerAndHandle { (poller, handle) =>
      val first: AnyRef = new Object
      val second: AnyRef = new Object
      CallbackBridge.store(poller, handle, first)
      CallbackBridge.store(poller, handle, second)
      assert(CallbackBridge.load[AnyRef](handle) eq second)
      CallbackBridge.clear(poller, handle)
    }
  }

  test("store anchors the holder in the poller's anchor map; clear removes it") {
    withPollerAndHandle { (poller, handle) =>
      val holder: AnyRef = new Object
      assertEquals(poller.anchors.size, 0)
      CallbackBridge.store(poller, handle, holder)
      assertEquals(poller.anchors.size, 1)
      CallbackBridge.clear(poller, handle)
      assertEquals(poller.anchors.size, 0)
    }
  }

end CallbackBridgeSpec
