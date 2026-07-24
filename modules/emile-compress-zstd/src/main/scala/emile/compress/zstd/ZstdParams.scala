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
package emile.compress.zstd

import emile.compress.Budget

/** Compression parameters for [[Encoder$ Encoder]]: the effort `level` (1..22). Build through
  * [[ZstdParams$ ZstdParams]] - [[ZstdParams.default]] is zstd's own level 3.
  */
final case class ZstdParams private (level: Int)

/** Factories and the preset for [[ZstdParams]]. */
object ZstdParams:

  /** zstd's default level 3 - a balanced speed/ratio compromise. */
  val default: ZstdParams = new ZstdParams(3)

  /** Compression at `level` (1..22). */
  def apply(level: Int): ZstdParams =
    require(level >= 1 && level <= 22, s"level must be in 1..22, was $level")
    new ZstdParams(level)

  given CanEqual[ZstdParams, ZstdParams] = CanEqual.derived

/** Decompression parameters for [[Decoder$ Decoder]]: the [[Budget]] every decompression must
  * carry, and `windowLogMax` - the base-2 log of the largest back-reference window the decoder will
  * allocate for, zstd's own memory guard (default 27, a 128 MiB ceiling). Build through
  * [[ZstdDecodeParams$ ZstdDecodeParams]].
  */
final case class ZstdDecodeParams private (budget: Budget, windowLogMax: Int)

/** Factories for [[ZstdDecodeParams]]. */
object ZstdDecodeParams:

  /** Enforce `budget`, keeping zstd's default 27-bit (128 MiB) window ceiling. */
  def apply(budget: Budget): ZstdDecodeParams = new ZstdDecodeParams(budget, 27)

  /** Enforce `budget` and cap the decoder window at 2^`windowLogMax` (10..31). */
  def apply(budget: Budget, windowLogMax: Int): ZstdDecodeParams =
    require(windowLogMax >= 10 && windowLogMax <= 31, s"windowLogMax must be in 10..31, was $windowLogMax")
    new ZstdDecodeParams(budget, windowLogMax)

  given CanEqual[ZstdDecodeParams, ZstdDecodeParams] = CanEqual.derived
