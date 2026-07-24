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

/** The output-byte ceiling every decompression enforces on its streaming loop - the only
  * format-universal defence against a decompression bomb, since no codec carries a trustworthy
  * in-band decompressed size. Represented as a `Long`: a non-negative byte limit, or
  * [[Budget.unlimited]]. Construct and query through [[Budget$ Budget]].
  */
opaque type Budget = Long

/** Factory and readers for [[Budget]]. Decompression rejects a stream once its cumulative output
  * exceeds the limit with [[CompressError.BudgetExceeded]]; [[Budget.unlimited]] is the explicit
  * opt-out.
  */
object Budget:

  /** No output-byte ceiling. An explicit choice: decompression will expand a hostile stream without
    * bound.
    */
  val unlimited: Budget = -1L

  /** A ceiling of `limit` output bytes. `limit` must be non-negative. */
  def bytes(limit: Long): Budget =
    require(limit >= 0, s"budget must be non-negative, was $limit")
    limit

  given CanEqual[Budget, Budget] = CanEqual.derived

  extension (budget: Budget)
    /** The byte ceiling, or a negative value when [[unlimited]]. */
    def limit: Long = budget

    /** Whether this budget imposes no ceiling. */
    def isUnlimited: Boolean = budget < 0L

    /** Whether `total` output bytes have crossed the ceiling ([[unlimited]] never has). */
    def exceededBy(total: Long): Boolean = budget >= 0L && total > budget
end Budget
