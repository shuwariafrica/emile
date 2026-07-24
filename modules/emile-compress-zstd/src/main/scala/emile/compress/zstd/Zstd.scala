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

/** Streaming zstd compression. The Level-1 [[Encoder.compress]] pipe compresses a whole stream; the
  * Level-2 [[Encoder.session]] exposes the incremental context with an explicit
  * [[Encoder.Session.flush]] and [[Encoder.Session.reset]]. Compression of valid input cannot fail,
  * so both carry an infallible channel.
  */
object Encoder:

  /** An incremental zstd encoder bound to one ZSTD_CStream. Drive it from a single fibre: the
    * context and its output scratch are mutable and hold no internal synchronisation.
    */
  final class Session private[zstd] (private[zstd] val cctx: Ptr[Byte], private[zstd] val scratch: Ptr[Byte]):

    /** Compress `input`, returning whatever output the encoder emits now (it may buffer for ratio). */
    def update(input: Slice): EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.encode(cctx, scratch, input, ZConst.EContinue)))

    /** Flush pending output without ending the frame. */
    def flush: EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.encode(cctx, scratch, Slice.of(scratch, 0), ZConst.EFlush)))

    /** Finish the frame, emitting any remaining output. */
    def finish: EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.encode(cctx, scratch, Slice.of(scratch, 0), ZConst.EEnd)))

    /** Reset the session, so the next input starts a fresh frame independent of this one. */
    def reset: EmIO[Nothing, Unit] =
      EffIO.liftF(IO { val _ = zstd.ZSTD_CCtx_reset(cctx, ZConst.ResetSessionOnly) })
  end Session

  /** A zstd encoder, released (freeing the context) when the resource closes. */
  def session(params: ZstdParams): EmResource[Nothing, Session] =
    Resource.make[EffIO.Of[Nothing], Session](acquire(params))(release)

  /** Compress a byte stream end to end under `params`, closing the frame. */
  def compress(params: ZstdParams): EmPipe[Nothing, Byte, Byte] =
    input =>
      Stream
        .resource(session(params))
        .flatMap: session =>
          input.chunks.evalMap(chunk => session.update(Native.chunkSlice(chunk))).flatMap(Stream.chunk) ++
            Stream.eval(session.finish).flatMap(Stream.chunk)

  private def acquire(params: ZstdParams): EmIO[Nothing, Session] =
    EffIO.liftF(
      IO(Native.createEncoder(params.level)).flatMap: cctx =>
        if Native.isNull(cctx) then IO.raiseError(CompressError.Unexpected(new IllegalStateException("zstd CStream allocation failed")))
        else IO(new Session(cctx, Native.scratch()))
    )

  private def release(session: Session): EmIO[Nothing, Unit] =
    EffIO.liftF(IO {
      val _ = zstd.ZSTD_freeCStream(session.cctx)
      Native.free(session.scratch)
    })
end Encoder

/** Streaming zstd decompression, enforcing the parameters' [[Budget]] on the output and the
  * `windowLogMax` memory cap on the frame. The Level-1 [[Decoder.decompress]] pipe decompresses a
  * whole stream; the Level-2 [[Decoder.session]] exposes the incremental context. Decompression
  * fails with a [[CompressError]] on a corrupt or over-large-window stream, a premature end, or a
  * budget breach.
  */
object Decoder:

  /** An incremental zstd decoder bound to one ZSTD_DStream. Drive it from a single fibre: the
    * context, its output scratch, and the running output total are mutable and unsynchronised.
    */
  final class Session private[zstd] (
    private[zstd] val dctx: Ptr[Byte],
    private[zstd] val scratch: Ptr[Byte],
    private[zstd] val budget: Budget):
    private[zstd] var total: Long = 0L // scalafix:ok DisableSyntax.var
    private[zstd] var finished: Boolean = false // scalafix:ok DisableSyntax.var

    /** Decompress `input`, enforcing the [[Budget]] on the cumulative output. */
    def update(input: Slice): EmIO[CompressError, Chunk[Byte]] =
      EffIO.delay(
        Native.decode(dctx, scratch, input, budget, total) match
          case Right(step) =>
            total = step.total
            finished = step.finished
            Right(step.output)
          case Left(error) => Left(error)
      )

    /** Reset the session, so the next input decodes a fresh frame independent of this one. */
    def reset: EmIO[Nothing, Unit] =
      EffIO.liftF(IO {
        val _ = zstd.ZSTD_DCtx_reset(dctx, ZConst.ResetSessionOnly)
        total = 0L
        finished = false
      })

    private[zstd] def finishCheck: EmIO[CompressError, Unit] =
      EffIO.delay(if finished then Right(()) else Left(CompressError.Truncated))
  end Session

  /** A zstd decoder, released (freeing the context) when the resource closes. */
  def session(params: ZstdDecodeParams): EmResource[Nothing, Session] =
    Resource.make[EffIO.Of[Nothing], Session](acquire(params))(release)

  /** Decompress a byte stream under `params`, failing with [[CompressError.Truncated]] if it ends
    * before the frame completes.
    */
  def decompress(params: ZstdDecodeParams): EmPipe[CompressError, Byte, Byte] =
    input =>
      Stream
        .resource(session(params).widen[CompressError])
        .flatMap: session =>
          input.chunks.evalMap(chunk => session.update(Native.chunkSlice(chunk))).flatMap(Stream.chunk) ++
            Stream.exec(session.finishCheck)

  private def acquire(params: ZstdDecodeParams): EmIO[Nothing, Session] =
    EffIO.liftF(
      IO(Native.createDecoder(params.windowLogMax)).flatMap: dctx =>
        if Native.isNull(dctx) then IO.raiseError(CompressError.Unexpected(new IllegalStateException("zstd DStream allocation failed")))
        else IO(new Session(dctx, Native.scratch(), params.budget))
    )

  private def release(session: Session): EmIO[Nothing, Unit] =
    EffIO.liftF(IO {
      val _ = zstd.ZSTD_freeDStream(session.dctx)
      Native.free(session.scratch)
    })
end Decoder
