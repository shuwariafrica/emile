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

import scala.scalanative.libc.stdlib
import scala.scalanative.libc.string
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import fs2.Chunk

import emile.compress.Budget
import emile.compress.CompressError

// One decode iteration's yield: the output produced, the running output total (for the budget), and
// whether the stream reached Z_STREAM_END.
final private[zlib] case class InflateStep(output: Chunk[Byte], total: Long, finished: Boolean)

// All zlib-ng FFI: stream allocation, the two drain loops, and the buffer conversions. Each session
// owns one zng_stream and one reusable output scratch; the drains fill the scratch and copy each
// yield into a right-sized array, so no borrowed pointer escapes.
private[zlib] object Native:

  def create(): Ptr[ZngStream] =
    val stream = stdlib.malloc(sizeof[ZngStream]).asInstanceOf[Ptr[ZngStream]] // scalafix:ok DisableSyntax.asInstanceOf
    val _ = string.memset(stream.asInstanceOf[Ptr[Byte]], 0, sizeof[ZngStream]) // scalafix:ok DisableSyntax.asInstanceOf
    stream

  def scratch(): Ptr[Byte] = stdlib.malloc(ZConst.ScratchSize.toCSize)

  def freeStream(stream: Ptr[ZngStream]): Unit = stdlib.free(stream.asInstanceOf[Ptr[Byte]]) // scalafix:ok DisableSyntax.asInstanceOf

  def free(pointer: Ptr[Byte]): Unit = stdlib.free(pointer)

  // Compress `input` under `flush` (Z_NO_FLUSH per chunk, Z_SYNC_FLUSH to emit a block boundary,
  // Z_FINISH to close the stream), draining until the codec stops filling the scratch.
  def deflate(stream: Ptr[ZngStream], scratchPtr: Ptr[Byte], input: Slice, flush: Int): Chunk[Byte] =
    stream._1 = input.unsafePtr
    stream._2 = input.length.toUInt
    deflateLoop(stream, scratchPtr, flush, Chunk.empty[Byte])

  @annotation.tailrec
  private def deflateLoop(stream: Ptr[ZngStream], scratchPtr: Ptr[Byte], flush: Int, acc: Chunk[Byte]): Chunk[Byte] =
    stream._4 = scratchPtr
    stream._5 = ZConst.ScratchSize.toUInt
    val _ = zlibng.zng_deflate(stream, flush)
    val produced = ZConst.ScratchSize - stream._5.toInt
    val next = if produced > 0 then acc ++ copyOut(scratchPtr, produced) else acc
    // avail_out == 0 means the scratch filled and the codec has more to emit for this input+flush.
    if stream._5.toInt == 0 then deflateLoop(stream, scratchPtr, flush, next) else next

  // Decompress `input`, enforcing `budget` on the cumulative output (`total0` carried from prior
  // calls). Left on a corrupt stream or a budget breach; Right otherwise, flagging Z_STREAM_END.
  def inflate(
    stream: Ptr[ZngStream],
    scratchPtr: Ptr[Byte],
    input: Slice,
    budget: Budget,
    total0: Long
  ): Either[CompressError, InflateStep] =
    stream._1 = input.unsafePtr
    stream._2 = input.length.toUInt
    inflateLoop(stream, scratchPtr, budget, total0, Chunk.empty[Byte])

  @annotation.tailrec
  private def inflateLoop(
    stream: Ptr[ZngStream],
    scratchPtr: Ptr[Byte],
    budget: Budget,
    total: Long,
    acc: Chunk[Byte]
  ): Either[CompressError, InflateStep] =
    stream._4 = scratchPtr
    stream._5 = ZConst.ScratchSize.toUInt
    val code = zlibng.zng_inflate(stream, ZConst.NoFlush)
    val produced = ZConst.ScratchSize - stream._5.toInt
    val next = if produced > 0 then acc ++ copyOut(scratchPtr, produced) else acc
    val total2 = total + produced
    if budget.exceededBy(total2) then Left(CompressError.BudgetExceeded(budget.limit))
    else
      code match
        case ZConst.StreamEnd => Right(InflateStep(next, total2, true))
        // avail_out == 0 means the scratch filled with more still to come for this input.
        case ZConst.Ok if stream._5.toInt == 0 => inflateLoop(stream, scratchPtr, budget, total2, next)
        case ZConst.Ok => Right(InflateStep(next, total2, false))
        case ZConst.BufError => Right(InflateStep(next, total2, false))
        case _ => Left(CompressError.Malformed(message(stream, code)))
  end inflateLoop

  private def copyOut(scratchPtr: Ptr[Byte], length: Int): Chunk[Byte] =
    val array = new Array[Byte](length)
    val _ = string.memcpy(array.atUnsafe(0), scratchPtr, length.toCSize)
    Chunk.array(array)

  def chunkSlice(chunk: Chunk[Byte]): Slice =
    val arraySlice = chunk.toArraySlice
    Slice.of(arraySlice.values, arraySlice.offset, arraySlice.length)

  private def message(stream: Ptr[ZngStream], code: Int): String =
    val text = stream._7
    if text == null then s"zlib-ng error code $code" // scalafix:ok DisableSyntax.null
    else fromCString(text)
end Native
