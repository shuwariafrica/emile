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

import scala.scalanative.unsafe.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk
import fs2.Stream

import emile.EmIO
import emile.EmPipe
import emile.EmResource
import emile.compress.Budget
import emile.compress.CompressError
import emile.widen

/** Streaming DEFLATE decompression over zlib-ng, expecting the [[Framing]] the parameters name and
  * enforcing their [[Budget]] on the output. The Level-1 [[Inflate.decompress]] pipe decompresses a
  * whole stream; the Level-2 [[Inflate.session]] exposes the incremental context that RFC 7692
  * permessage-deflate drives with [[Inflate.Session.reset]] per message.
  *
  * Decompression fails with a [[CompressError]] on a corrupt stream, a premature end, or a budget
  * breach.
  */
object Inflate:

  /** An incremental inflate context bound to one zlib-ng stream. Drive it from a single fibre: the
    * native stream, its output scratch, and the running output total are mutable and
    * unsynchronised.
    */
  final class Session private[zlib] (
    private[zlib] val stream: Ptr[ZngStream],
    private[zlib] val scratch: Ptr[Byte],
    private[zlib] val budget: Budget
  ):
    private[zlib] var total: Long = 0L // scalafix:ok DisableSyntax.var
    private[zlib] var finished: Boolean = false // scalafix:ok DisableSyntax.var

    /** Decompress `input`, returning the output it yields and enforcing the [[Budget]] on the
      * cumulative total across every call.
      */
    def update(input: Slice): EmIO[CompressError, Chunk[Byte]] =
      EffIO.delay(
        Native.inflate(stream, scratch, input, budget, total) match
          case Right(step) =>
            total = step.total
            finished = step.finished
            Right(step.output)
          case Left(error) => Left(error)
      )

    /** Reset to an empty LZ77 window and clear the budget tally, so the next message decompresses
      * independently - the `no_context_takeover` behaviour RFC 7692 requires.
      */
    def reset: EmIO[Nothing, Unit] =
      EffIO.liftF(IO {
        val _ = zlibng.zng_inflateReset(stream)
        total = 0L
        finished = false
      })

    // Fails with Truncated when the pipe's input ended before the codec reached Z_STREAM_END.
    private[zlib] def finishCheck: EmIO[CompressError, Unit] =
      EffIO.delay(if finished then Right(()) else Left(CompressError.Truncated))
  end Session

  /** An inflate context, released (freeing the native stream) when the resource closes. */
  def session(params: InflateParams): EmResource[Nothing, Session] =
    Resource.make[EffIO.Of[Nothing], Session](acquire(params))(release)

  /** Decompress a byte stream under `params`, failing with [[CompressError.Truncated]] if it ends
    * before the DEFLATE stream completes.
    */
  def decompress(params: InflateParams): EmPipe[CompressError, Byte, Byte] =
    input =>
      Stream
        .resource(session(params).widen[CompressError])
        .flatMap: session =>
          input.chunks.evalMap(chunk => session.update(Native.chunkSlice(chunk))).flatMap(Stream.chunk) ++
            Stream.exec(session.finishCheck)

  private def acquire(params: InflateParams): EmIO[Nothing, Session] =
    EffIO.liftF(
      IO(Native.create()).flatMap: stream =>
        IO(zlibng.zng_inflateInit2(stream, Framing.bits(params.framing)))
          .flatMap: code =>
            if code == ZConst.Ok then IO(new Session(stream, Native.scratch(), params.budget))
            else
              IO(Native.freeStream(stream)) *>
                IO.raiseError(CompressError.Unexpected(new IllegalStateException(s"zlib-ng inflateInit2 failed with code $code")))
    )

  private def release(session: Session): EmIO[Nothing, Unit] =
    EffIO.liftF(IO {
      val _ = zlibng.zng_inflateEnd(session.stream)
      Native.free(session.scratch)
      Native.freeStream(session.stream)
    })
end Inflate
