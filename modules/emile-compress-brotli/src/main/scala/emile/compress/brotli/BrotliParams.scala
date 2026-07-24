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

import emile.compress.Budget

/** The brotli context model, biasing the entropy coder for the expected input. [[Mode.Generic]]
  * suits arbitrary bytes; [[Mode.Text]] and [[Mode.Font]] tune for UTF-8 text and WOFF2 fonts.
  */
enum Mode derives CanEqual:
  case Generic, Text, Font

object Mode:
  private[brotli] def code(mode: Mode): Int = mode match
    case Generic => 0
    case Text => 1
    case Font => 2

/** Compression parameters for [[Encoder$ Encoder]]: `quality` (0..11), the window `lgWin` (10..24),
  * and the [[Mode]]. Build through [[BrotliParams$ BrotliParams]] - [[BrotliParams.default]] is a
  * streaming preset and [[BrotliParams.maximum]] is the library's maximum-effort default.
  */
final case class BrotliParams private (quality: Int, lgWin: Int, mode: Mode)

/** Factories and presets for [[BrotliParams]]. */
object BrotliParams:

  /** Quality 5 at the default 22-bit window - a streaming compromise. Brotli's own default quality
    * is 11, whose match search is far slower per byte than a response path should pay; quality 5
    * keeps the ratio strong at a fraction of the cost.
    */
  val default: BrotliParams = new BrotliParams(5, 22, Mode.Generic)

  /** Quality 11 at the default 22-bit window - the library's maximum-effort default, named for what
    * it costs. Reserve it for once-compressed, many-times-served payloads.
    */
  val maximum: BrotliParams = new BrotliParams(11, 22, Mode.Generic)

  /** `quality` (0..11) at the default window and [[Mode.Generic]]. */
  def apply(quality: Int): BrotliParams = apply(quality, 22, Mode.Generic)

  /** `quality` (0..11), window `lgWin` (10..24), and `mode`. */
  def apply(quality: Int, lgWin: Int, mode: Mode): BrotliParams =
    require(quality >= 0 && quality <= 11, s"quality must be in 0..11, was $quality")
    require(lgWin >= 10 && lgWin <= 24, s"lgWin must be in 10..24, was $lgWin")
    new BrotliParams(quality, lgWin, mode)

  given CanEqual[BrotliParams, BrotliParams] = CanEqual.derived
end BrotliParams

/** Decompression parameters for [[Decoder$ Decoder]]: the [[Budget]] every decompression must
  * carry, and whether to accept brotli large-window streams (windows above 16 MiB, up to 1 GiB) -
  * off by default, since a large window is itself a memory-amplification vector. Build through
  * [[BrotliDecodeParams$ BrotliDecodeParams]].
  */
final case class BrotliDecodeParams private (budget: Budget, largeWindow: Boolean)

/** Factories for [[BrotliDecodeParams]]. */
object BrotliDecodeParams:

  /** Enforce `budget`, rejecting large-window streams. */
  def apply(budget: Budget): BrotliDecodeParams = new BrotliDecodeParams(budget, false)

  /** Enforce `budget`, accepting large-window streams when `largeWindow` is set. */
  def apply(budget: Budget, largeWindow: Boolean): BrotliDecodeParams = new BrotliDecodeParams(budget, largeWindow)

  given CanEqual[BrotliDecodeParams, BrotliDecodeParams] = CanEqual.derived
