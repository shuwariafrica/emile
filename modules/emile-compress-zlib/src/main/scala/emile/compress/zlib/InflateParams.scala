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

import emile.compress.Budget

/** Decompression parameters for [[Inflate$ Inflate]]: the [[Framing]] envelope to expect and the
  * [[Budget]] every decompression must carry - the output-byte ceiling that guards against a
  * decompression bomb. Build through [[InflateParams$ InflateParams]].
  */
final case class InflateParams private (framing: Framing, budget: Budget)

/** Factory for [[InflateParams]]. */
object InflateParams:

  /** Expect `framing`, enforcing `budget` on the decompressed output. */
  def apply(framing: Framing, budget: Budget): InflateParams = new InflateParams(framing, budget)

  given CanEqual[InflateParams, InflateParams] = CanEqual.derived
