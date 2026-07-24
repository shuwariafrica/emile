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
package emile.compress.zlib

/** The DEFLATE match-finder strategy (zlib's `strategy` argument). [[Strategy.Default]] suits most
  * data; the others tune the entropy coder for specific inputs.
  */
enum Strategy derives CanEqual:
  case Default, Filtered, HuffmanOnly, Rle, Fixed

object Strategy:
  private[zlib] def code(strategy: Strategy): Int = strategy match
    case Default => 0
    case Filtered => 1
    case HuffmanOnly => 2
    case Rle => 3
    case Fixed => 4

/** Compression parameters for [[Deflate$ Deflate]]: the [[Framing]] envelope, the effort `level`
  * (0..9), the `memLevel` state-memory trade (1..9), and the [[Strategy]]. Build through
  * [[DeflateParams$ DeflateParams]] - [[DeflateParams.default]] is a balanced streaming preset.
  */
final case class DeflateParams private (framing: Framing, level: Int, memLevel: Int, strategy: Strategy)

/** Factories and the preset for [[DeflateParams]]. */
object DeflateParams:

  /** Zlib framing, level 6, memLevel 8, default strategy - zlib's own speed/ratio compromise. */
  val default: DeflateParams = new DeflateParams(Framing.Zlib, 6, 8, Strategy.Default)

  /** `framing` at the [[default]] level, memLevel, and strategy. */
  def apply(framing: Framing): DeflateParams = new DeflateParams(framing, 6, 8, Strategy.Default)

  /** `framing` at `level` (0..9), with the default memLevel and strategy. */
  def apply(framing: Framing, level: Int): DeflateParams = apply(framing, level, 8, Strategy.Default)

  /** `framing` at `level` (0..9), `memLevel` (1..9), and `strategy`. */
  def apply(framing: Framing, level: Int, memLevel: Int, strategy: Strategy): DeflateParams =
    require(level >= 0 && level <= 9, s"level must be in 0..9, was $level")
    require(memLevel >= 1 && memLevel <= 9, s"memLevel must be in 1..9, was $memLevel")
    new DeflateParams(framing, level, memLevel, strategy)

  given CanEqual[DeflateParams, DeflateParams] = CanEqual.derived
end DeflateParams
