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

import scala.util.control.NoStackTrace

/** Sealed root of every typed error the compression codecs surface. It extends `Exception` so the
  * value an [[emile.EmIO EmIO]] channel carries into cats-effect's `Throwable` channel is the error
  * itself, still recoverable and matchable; `getMessage` is derived per case.
  *
  * The named cases live in [[CompressError$ CompressError]].
  */
@scala.annotation.internal.sharable
sealed abstract class CompressError(message: String, cause: Option[Throwable])
    extends Exception(message, cause.orNull[Throwable | Null])
    with NoStackTrace derives CanEqual

/** The [[CompressError]] cases: [[CompressError.Malformed Malformed]] (a corrupt or unsupported
  * stream, carrying the codec's own error text), [[CompressError.Truncated Truncated]] (input ended
  * before the stream completed), [[CompressError.BudgetExceeded BudgetExceeded]] (decompressed
  * output crossed the configured [[Budget]]), and an idempotent
  * [[CompressError.Unexpected Unexpected]] wrapping a raw `Throwable` - an already-typed cause is
  * returned unchanged.
  *
  * Every case is a type as well as a value, so a decompression channel narrows to the failures an
  * operation can actually produce.
  */
object CompressError:

  // Payload-free cases use the class-plus-case-object shape: a union arm named by a bare singleton
  // type mis-erases on Scala 3.8.4, so type positions must name a class.

  /** A corrupt, incomplete-per-format, or unsupported compressed stream; `detail` carries the
    * codec's own error text.
    */
  final case class Malformed(detail: String) extends CompressError(s"Malformed compressed stream: $detail", None)

  /** The input ended while the codec still expected more of the stream. */
  sealed abstract class Truncated private () extends CompressError("Compressed stream ended before it completed", None)
  case object Truncated extends Truncated

  /** Decompressed output crossed the `limit`-byte ceiling the operation's [[Budget]] set. */
  final case class BudgetExceeded(limit: Long) extends CompressError(s"Decompressed output exceeded the $limit-byte budget", None)

  /** A defect raised through the `Throwable` channel: an unexpected failure of an otherwise
    * infallible codec call. Idempotent - wrapping an existing [[CompressError]] returns it
    * unchanged.
    */
  final class Unexpected private (val cause: Throwable) extends CompressError("", Some(cause)):
    override def getMessage: String = s"Unexpected compression failure: ${cause.getMessage}"

  object Unexpected:
    def apply(cause: Throwable): CompressError = cause match
      case e: CompressError => e
      case t => new Unexpected(t)

    def unapply(u: Unexpected): Some[Throwable] = Some(u.cause)
end CompressError
