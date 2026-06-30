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
import scala.scalanative.unsigned.*

import cats.effect.IO

import emile.EmileSuite
import emile.LibUVPollingSystem

/** Covers [[LiveHandle]]: the guard runs the thunk while the handle is live and returns its result;
  * once [[LiveHandle.closeOnOwner]] has freed the handle the guard short-circuits to the closed
  * value without dereferencing the freed pointer; and a repeated close is a harmless no-op rather
  * than a native double free. Exercised against a live loop because reclamation completes only when
  * libuv's `uv_close` callback fires under a running `uv_run`.
  */
final class LiveHandleSpec extends EmileSuite:

  test("tryUse runs the thunk while the handle is live and yields its result") {
    withLiveHandle((poller, live) => routed(poller)(LiveHandle.tryUse(live, -1)(_ => 42))).assertEquals(42)
  }

  test("tryUse short-circuits to the closed value after closeOnOwner, never touching the freed handle") {
    withLiveHandle { (poller, live) =>
      LiveHandle.closeOnOwner(live) >> routed(poller)(LiveHandle.tryUse(live, -1)(_ => 42))
    }.assertEquals(-1)
  }

  test("closeOnOwner is idempotent - a repeated close is a no-op, never a native double free") {
    withLiveHandle { (poller, live) =>
      LiveHandle.closeOnOwner(live) >> LiveHandle.closeOnOwner(live) >> routed(poller)(LiveHandle.tryUse(live, true)(_ => false))
    }.assertEquals(true)
  }

  test("poller reports the loop the handle was created on") {
    withLiveHandle((poller, live) => IO(LiveHandle.poller(live) eq poller)).assertEquals(true)
  }

  // Acquire a fresh handle on the calling worker's loop, run use, then always reclaim it - the
  // reclaim is idempotent, so a test that also closes explicitly stays leak-free.
  private def withLiveHandle[A](use: (LibUVPoller, LiveHandle) => IO[A]): IO[A] =
    LibUVPollingSystem.currentPoller.flatMap: poller =>
      routed(poller)(makeHandle(poller)).flatMap(live => use(poller, live).guarantee(LiveHandle.closeOnOwner(live)))

  // Run a synchronous thunk through the affinity router - the owner-thread context tryUse requires.
  private def routed[A](poller: LibUVPoller)(thunk: => A): IO[A] = Routing.onOwner(poller)(thunk)

  // A live uv_timer_t (the simplest handle that needs no init callback) wrapped as an acquire would.
  private def makeHandle(poller: LibUVPoller): LiveHandle =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_TIMER))
    LibUV.uv_timer_init(poller.loop, handle): Unit
    LiveHandle(poller, handle)

end LiveHandleSpec
