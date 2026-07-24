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

/** Which of the three DEFLATE envelopes a stream carries - the `windowBits` selector of RFC
  * 1950/1951/1952. [[Framing.Zlib]] is the zlib wrapper (RFC 1950, Adler-32), [[Framing.Gzip]] the
  * gzip wrapper (RFC 1952, CRC-32), [[Framing.Raw]] headerless DEFLATE (RFC 1951) with a negotiable
  * window - the mode RFC 7692 permessage-deflate rides. Construct [[Framing.Raw]] through
  * [[Framing$ Framing]].
  */
sealed trait Framing

/** The [[Framing]] cases and the validated [[Framing.raw]] factory. */
object Framing:

  /** The zlib wrapper (RFC 1950): a 2-byte header and an Adler-32 trailer, 32 KiB window. */
  case object Zlib extends Framing

  /** The gzip wrapper (RFC 1952): a gzip header and a CRC-32 plus length trailer, 32 KiB window. */
  case object Gzip extends Framing

  /** Headerless DEFLATE (RFC 1951) with an LZ77 window of 2^`windowBits` bytes. `windowBits` is in
    * 8..15; both peers must agree it, as a raw stream carries no window descriptor.
    */
  final case class Raw private[Framing] (windowBits: Int) extends Framing

  /** A [[Raw]] framing with the given `windowBits` (8..15 - the RFC 7692 negotiation range).
    *
    * zlib-ng promotes a request for 8 to 9 (a 512-byte minimum window), so `Raw(8)` and a peer's
    * `Raw(8)` interoperate at a 2^9 window; the promotion is symmetric because a raw stream fixes
    * the window on both sides from this value.
    */
  def raw(windowBits: Int): Framing =
    require(windowBits >= 8 && windowBits <= 15, s"raw windowBits must be in 8..15, was $windowBits")
    new Raw(windowBits)

  given CanEqual[Framing, Framing] = CanEqual.derived

  // The signed windowBits argument to zng_{deflate,inflate}Init2: positive for a zlib wrapper, +16
  // for gzip, negative for raw.
  private[zlib] def bits(framing: Framing): Int = framing match
    case Zlib => 15
    case Gzip => 31
    case Raw(w) => -w
end Framing
