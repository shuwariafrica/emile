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
import emile.compress.CompressError

/** Streaming DEFLATE compression over zlib-ng, in the [[Framing]] the parameters select. The
  * Level-1 [[Deflate.compress]] pipe compresses a whole byte stream; the Level-2
  * [[Deflate.session]] exposes the underlying incremental context for bespoke framing - notably RFC
  * 7692 permessage-deflate, which drives [[Deflate.Session.flush]] and [[Deflate.Session.reset]]
  * per message.
  *
  * Compression of valid input cannot fail, so both surfaces carry an infallible error channel.
  */
object Deflate:

  /** An incremental DEFLATE context bound to one zlib-ng stream. Drive it from a single fibre: the
    * native stream and its output scratch are mutable and hold no internal synchronisation.
    */
  final class Session private[zlib] (private[zlib] val stream: Ptr[ZngStream], private[zlib] val scratch: Ptr[Byte]):

    /** Compress `input`, returning whatever output the codec emits now (it may buffer for ratio). */
    def update(input: Slice): EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.deflate(stream, scratch, input, ZConst.NoFlush)))

    /** Flush pending output at a byte boundary with Z_SYNC_FLUSH; the emitted block ends with the
      * `00 00 ff ff` empty stored block RFC 7692 strips.
      */
    def flush: EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.deflate(stream, scratch, Slice.of(scratch, 0), ZConst.SyncFlush)))

    /** Finish the stream with Z_FINISH, emitting the trailer and any remaining output. */
    def finish: EmIO[Nothing, Chunk[Byte]] =
      EffIO.liftF(IO(Native.deflate(stream, scratch, Slice.of(scratch, 0), ZConst.Finish)))

    /** Reset to an empty LZ77 window, so the next message compresses independently of this one -
      * the `no_context_takeover` behaviour RFC 7692 requires.
      */
    def reset: EmIO[Nothing, Unit] =
      EffIO.liftF(IO { val _ = zlibng.zng_deflateReset(stream) })
  end Session

  /** A DEFLATE compression context, released (freeing the native stream) when the resource closes. */
  def session(params: DeflateParams): EmResource[Nothing, Session] =
    Resource.make[EffIO.Of[Nothing], Session](acquire(params))(release)

  /** Compress a byte stream end to end under `params`, closing it with Z_FINISH. */
  def compress(params: DeflateParams): EmPipe[Nothing, Byte, Byte] =
    input =>
      Stream
        .resource(session(params))
        .flatMap: session =>
          input.chunks.evalMap(chunk => session.update(Native.chunkSlice(chunk))).flatMap(Stream.chunk) ++
            Stream.eval(session.finish).flatMap(Stream.chunk)

  private def acquire(params: DeflateParams): EmIO[Nothing, Session] =
    EffIO.liftF(
      IO(Native.create()).flatMap: stream =>
        IO(
          zlibng.zng_deflateInit2(
            stream,
            params.level,
            ZConst.Deflated,
            Framing.bits(params.framing),
            params.memLevel,
            Strategy.code(params.strategy)
          )
        )
          .flatMap: code =>
            if code == ZConst.Ok then IO(new Session(stream, Native.scratch()))
            else
              IO(Native.freeStream(stream)) *>
                IO.raiseError(CompressError.Unexpected(new IllegalStateException(s"zlib-ng deflateInit2 failed with code $code")))
    )

  private def release(session: Session): EmIO[Nothing, Unit] =
    EffIO.liftF(IO {
      val _ = zlibng.zng_deflateEnd(session.stream)
      Native.free(session.scratch)
      Native.freeStream(session.stream)
    })
end Deflate
