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
import emile.compress.CompressError
import emile.widen

/** Streaming brotli compression. The Level-1 [[Encoder.compress]] pipe compresses a whole stream;
  * the Level-2 [[Encoder.session]] exposes the incremental context with an explicit
  * [[Encoder.Session.flush]]. Compression of valid input cannot fail, so both carry an infallible
  * channel.
  *
  * Brotli has no encoder reset: a fresh message is a fresh session.
  */
object Encoder:

  /** An incremental brotli encoder bound to one native state. Drive it from a single fibre: the
    * state and its output scratch are mutable and hold no internal synchronisation.
    */
  final class Session private[brotli] (private[brotli] val state: Ptr[Byte], private[brotli] val scratch: Ptr[Byte]):

    /** Compress `input`, returning whatever output the encoder emits now (it may buffer for ratio). */
    def update(input: Slice): EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.encode(state, scratch, input, BConst.OpProcess)))

    /** Flush pending output at a byte boundary without ending the stream. */
    def flush: EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.encode(state, scratch, Slice.of(scratch, 0), BConst.OpFlush)))

    /** Finish the stream, emitting any remaining output. */
    def finish: EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.encode(state, scratch, Slice.of(scratch, 0), BConst.OpFinish)))

  /** A brotli encoder, released (freeing the native state) when the resource closes. */
  def session(params: BrotliParams): EmResource[Nothing, Session] =
    Resource.make[EffIO.Of[Nothing], Session](acquire(params))(release)

  /** Compress a byte stream end to end under `params`. */
  def compress(params: BrotliParams): EmPipe[Nothing, Byte, Byte] =
    input =>
      Stream
        .resource(session(params))
        .flatMap: session =>
          input.chunks.evalMap(chunk => session.update(Native.chunkSlice(chunk))).flatMap(Stream.chunk) ++
            Stream.eval(session.finish).flatMap(Stream.chunk)

  private def acquire(params: BrotliParams): EmIO[Nothing, Session] =
    EffIO.liftF(
      IO(Native.createEncoder(params)).flatMap: state =>
        if Native.isNull(state) then IO.raiseError(CompressError.Unexpected(new IllegalStateException("brotli encoder allocation failed")))
        else IO(new Session(state, Native.scratch()))
    )

  private def release(session: Session): EmIO[Nothing, Unit] =
    EffIO.liftF(IO {
      brotli.BrotliEncoderDestroyInstance(session.state)
      Native.free(session.scratch)
    })
end Encoder

/** Streaming brotli decompression, enforcing the parameters' [[emile.compress.Budget Budget]] on
  * the output. The Level-1 [[Decoder.decompress]] pipe decompresses a whole stream; the Level-2
  * [[Decoder.session]] exposes the incremental context. Decompression fails with a
  * [[CompressError]] on a corrupt stream, a premature end, or a budget breach.
  */
object Decoder:

  /** An incremental brotli decoder bound to one native state. Drive it from a single fibre: the
    * state, its output scratch, and the running output total are mutable and unsynchronised.
    */
  final class Session private[brotli] (
    private[brotli] val state: Ptr[Byte],
    private[brotli] val scratch: Ptr[Byte],
    private[brotli] val budget: emile.compress.Budget):
    private[brotli] var total: Long = 0L // scalafix:ok DisableSyntax.var
    private[brotli] var finished: Boolean = false // scalafix:ok DisableSyntax.var

    /** Decompress `input`, enforcing the [[emile.compress.Budget Budget]] on the cumulative output. */
    def update(input: Slice): EmIO[CompressError, Chunk[Byte]] =
      EffIO.delay(
        Native.decode(state, scratch, input, budget, total) match
          case Right(step) =>
            total = step.total
            finished = step.finished
            Right(step.output)
          case Left(error) => Left(error)
      )

    private[brotli] def finishCheck: EmIO[CompressError, Unit] =
      EffIO.delay(if finished then Right(()) else Left(CompressError.Truncated))
  end Session

  /** A brotli decoder, released (freeing the native state) when the resource closes. */
  def session(params: BrotliDecodeParams): EmResource[Nothing, Session] =
    Resource.make[EffIO.Of[Nothing], Session](acquire(params))(release)

  /** Decompress a byte stream under `params`, failing with [[CompressError.Truncated]] if it ends
    * before the brotli stream completes.
    */
  def decompress(params: BrotliDecodeParams): EmPipe[CompressError, Byte, Byte] =
    input =>
      Stream
        .resource(session(params).widen[CompressError])
        .flatMap: session =>
          input.chunks.evalMap(chunk => session.update(Native.chunkSlice(chunk))).flatMap(Stream.chunk) ++
            Stream.exec(session.finishCheck)

  private def acquire(params: BrotliDecodeParams): EmIO[Nothing, Session] =
    EffIO.liftF(
      IO(Native.createDecoder(params.largeWindow)).flatMap: state =>
        if Native.isNull(state) then IO.raiseError(CompressError.Unexpected(new IllegalStateException("brotli decoder allocation failed")))
        else IO(new Session(state, Native.scratch(), params.budget))
    )

  private def release(session: Session): EmIO[Nothing, Unit] =
    EffIO.liftF(IO {
      brotli.BrotliDecoderDestroyInstance(session.state)
      Native.free(session.scratch)
    })
end Decoder
