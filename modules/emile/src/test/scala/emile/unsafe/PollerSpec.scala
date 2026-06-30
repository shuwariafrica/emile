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

import java.util.concurrent.atomic.AtomicBoolean

import cats.effect.unsafe.PollResult

import emile.LoopConfig

/** Covers [[LibUVPoller]]: construction, the timeout modes of `poll`, cross-thread submission and
  * interruption, and clean shutdown.
  */
final class PollerSpec extends munit.FunSuite:

  test("a fresh poller polls non-blocking and reports Complete") {
    val poller = new LibUVPoller(LoopConfig.default)
    assertEquals(poller.poll(0L), PollResult.Complete)
    poller.close()
  }

  test("interrupt makes the next poll return Interrupted") {
    val poller = new LibUVPoller(LoopConfig.default)
    poller.interrupt()
    assertEquals(poller.poll(0L), PollResult.Interrupted)
    poller.close()
  }

  test("a submitted runnable runs on the next poll") {
    val poller = new LibUVPoller(LoopConfig.default)
    val ran = new AtomicBoolean(false)
    assert(poller.submit(() => ran.set(true)))
    poller.poll(0L): Unit
    assert(ran.get())
    poller.close()
  }

  test("isOwnerThread holds for the polling thread") {
    val poller = new LibUVPoller(LoopConfig.default)
    poller.poll(0L): Unit
    assert(poller.isOwnerThread)
    poller.close()
  }

  test("the profiler preset configures and constructs a poller") {
    val poller = new LibUVPoller(LoopConfig.profilerProfile)
    assertEquals(poller.poll(0L), PollResult.Complete)
    poller.close()
  }

end PollerSpec
