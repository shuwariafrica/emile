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

import boilerplate.Slice

// Random: filling a caller buffer, allocating fresh bytes, and the non-negative-count pre-flight.
final class RandomSpec extends EmileSuite:

  test("fill populates a caller buffer with varied bytes") {
    val buffer = new Array[Byte](64)
    Random
      .fill(Slice.of(buffer))
      .absolve
      .map: _ =>
        assert(buffer.exists(_ != 0.toByte), "fill left the buffer all zero")
        assert(buffer.toSet.size > 10, "fill produced implausibly few distinct byte values")
  }

  test("bytes allocates a fresh array of the requested length") {
    Random
      .bytes(48)
      .absolve
      .map: bytes =>
        assertEquals(bytes.length, 48)
        assert(bytes.toSet.size > 10, "bytes produced implausibly few distinct byte values")
  }

  test("bytes of zero is an empty array and fill of an empty slice is a no-op") {
    for
      empty <- Random.bytes(0).absolve
      _ <- Random.fill(Slice.empty).absolve
    yield assertEquals(empty.length, 0)
  }

  test("a negative byte count is rejected before reaching libuv") {
    Random.bytes(-1).either.map {
      case Left(_: EmileError.IO.InvalidArgument) => ()
      case other => fail(s"expected InvalidArgument, got: $other")
    }
  }

  test("two fills differ, a sanity check that the buffer is actually randomised") {
    for
      first <- Random.bytes(32).absolve
      second <- Random.bytes(32).absolve
    yield assert(!first.sameElements(second), "two random draws were identical")
  }

end RandomSpec
