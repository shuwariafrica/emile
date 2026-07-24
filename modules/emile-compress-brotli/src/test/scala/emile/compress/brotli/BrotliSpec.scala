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
package emile.compress.brotli

import boilerplate.Slice
import cats.effect.IO
import fs2.Stream

import emile.compress.Budget
import emile.compress.CompressError
import emile.widen

// Round-trips both surfaces across several qualities and both presets, and exercises the budget
// guard arms and the corrupt/truncated failures.
final class BrotliSpec extends munit.CatsEffectSuite:

  private def sample(size: Int): Array[Byte] =
    Array.tabulate(size)(i => (32 + (i * 31 + (i / 7)) % 90).toByte)

  private def compressed(params: BrotliParams, input: Array[Byte]): IO[Vector[Byte]] =
    Stream.emits(input.toIndexedSeq).through(Encoder.compress(params)).compile.toVector.absolve

  private def decompressed(params: BrotliDecodeParams, input: Vector[Byte]): IO[Vector[Byte]] =
    Stream.emits(input).through(Decoder.decompress(params)).compile.toVector.absolve

  for params <- List(BrotliParams(1), BrotliParams.default, BrotliParams.maximum) do
    test(s"pipe round-trip at quality ${params.quality}") {
      val data = sample(20000)
      compressed(params, data)
        .flatMap(bytes => decompressed(BrotliDecodeParams(Budget.unlimited), bytes))
        .map(out => assertEquals(out, data.toVector))
    }

  test("session round-trip: encode then decode reproduce the input") {
    val data = sample(12000)
    val encode = Encoder
      .session(BrotliParams.default)
      .use(session =>
        for
          body <- session.update(Slice.of(data))
          tail <- session.finish
        yield (body ++ tail).toVector
      )
      .absolve
    encode
      .flatMap(bytes =>
        Decoder
          .session(BrotliDecodeParams(Budget.unlimited))
          .widen[CompressError]
          .use(session => session.update(Slice.of(bytes.toArray)))
          .absolve
      )
      .map(out => assertEquals(out.toVector, data.toVector))
  }

  test("budget: a limit equal to the output succeeds, one below aborts") {
    val data = sample(50000)
    compressed(BrotliParams.default, data).flatMap { bytes =>
      val exact = decompressed(BrotliDecodeParams(Budget.bytes(data.length.toLong)), bytes)
        .map(out => assertEquals(out.length, data.length))
      val below = decompressed(BrotliDecodeParams(Budget.bytes(data.length.toLong - 1)), bytes).attempt.map {
        case Left(error: CompressError.BudgetExceeded) => assertEquals(error.limit, data.length.toLong - 1)
        case other => fail(s"expected BudgetExceeded, got $other")
      }
      exact *> below
    }
  }

  test("budget: a small ceiling aborts a highly compressible stream mid-decode") {
    val data = Array.fill(1 << 20)(0.toByte)
    compressed(BrotliParams.default, data).flatMap { bytes =>
      decompressed(BrotliDecodeParams(Budget.bytes(1024)), bytes).attempt.map {
        case Left(_: CompressError.BudgetExceeded) => ()
        case other => fail(s"expected BudgetExceeded, got $other")
      }
    }
  }

  test("truncated input fails with Truncated") {
    val data = sample(40000)
    compressed(BrotliParams.default, data).flatMap { bytes =>
      decompressed(BrotliDecodeParams(Budget.unlimited), bytes.take(bytes.length / 2)).attempt.map {
        case Left(_: CompressError.Truncated) => ()
        case other => fail(s"expected Truncated, got $other")
      }
    }
  }

  test("corrupt input fails with Malformed") {
    val garbage = Vector.tabulate(64)(i => (i * 91 + 17).toByte)
    decompressed(BrotliDecodeParams(Budget.unlimited), garbage).attempt.map {
      case Left(_: CompressError.Malformed) => ()
      case other => fail(s"expected Malformed, got $other")
    }
  }
end BrotliSpec
