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
package emile.compress

import cats.effect.IO
import fs2.Stream

import emile.EmPipe
import emile.compress.brotli.BrotliDecodeParams
import emile.compress.brotli.BrotliParams
import emile.compress.brotli.Decoder as BrotliDecoder
import emile.compress.brotli.Encoder as BrotliEncoder
import emile.compress.zlib.Deflate
import emile.compress.zlib.DeflateParams
import emile.compress.zlib.Framing
import emile.compress.zlib.Inflate
import emile.compress.zlib.InflateParams
import emile.compress.zstd.Decoder as ZstdDecoder
import emile.compress.zstd.Encoder as ZstdEncoder
import emile.compress.zstd.ZstdDecodeParams
import emile.compress.zstd.ZstdParams

// The suite-level co-link guarantee: zlib-ng, brotli, and zstd archives all link into one test
// binary and each round-trips, proving the vendored families carry no colliding symbols.
final class CoLinkSpec extends munit.CatsEffectSuite:

  private def roundTrip(
    compress: EmPipe[Nothing, Byte, Byte],
    decompress: EmPipe[CompressError, Byte, Byte],
    input: Array[Byte]
  ): IO[Vector[Byte]] =
    Stream.emits(input.toIndexedSeq).through(compress).through(decompress).compile.toVector.absolve

  test("zlib-ng, brotli, and zstd all round-trip in one binary") {
    val data = Array.tabulate(30000)(i => (32 + (i * 37 + i / 5) % 90).toByte)
    for
      viaZlib <- roundTrip(
                   Deflate.compress(DeflateParams.default),
                   Inflate.decompress(InflateParams(Framing.Zlib, Budget.unlimited)),
                   data
                 )
      viaBrotli <- roundTrip(
                     BrotliEncoder.compress(BrotliParams.default),
                     BrotliDecoder.decompress(BrotliDecodeParams(Budget.unlimited)),
                     data
                   )
      viaZstd <- roundTrip(
                   ZstdEncoder.compress(ZstdParams.default),
                   ZstdDecoder.decompress(ZstdDecodeParams(Budget.unlimited)),
                   data
                 )
    yield
      assertEquals(viaZlib, data.toVector)
      assertEquals(viaBrotli, data.toVector)
      assertEquals(viaZstd, data.toVector)
    end for
  }
end CoLinkSpec
