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

import scala.scalanative.libc.string
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import fs2.Chunk

import emile.compress.Budget
import emile.compress.CompressError

// One decode iteration's yield: the output produced, the running output total (for the budget), and
// whether the decoder reported BROTLI_DECODER_RESULT_SUCCESS.
final private[brotli] case class DecodeStep(output: Chunk[Byte], total: Long, finished: Boolean)

// All brotli FFI: state creation, the two cursor-driven drain loops, and the buffer conversions.
private[brotli] object Native:

  // scalafix:off DisableSyntax
  private def nullPtr: Ptr[Byte] = null.asInstanceOf[Ptr[Byte]]
  // scalafix:on DisableSyntax

  def createEncoder(params: BrotliParams): Ptr[Byte] =
    val state = brotli.BrotliEncoderCreateInstance(nullPtr, nullPtr, nullPtr)
    if state != null then // scalafix:ok DisableSyntax.null
      val _ = brotli.BrotliEncoderSetParameter(state, BConst.ParamQuality, params.quality.toUInt)
      val _ = brotli.BrotliEncoderSetParameter(state, BConst.ParamLgWin, params.lgWin.toUInt)
      val _ = brotli.BrotliEncoderSetParameter(state, BConst.ParamMode, Mode.code(params.mode).toUInt)
    state

  def createDecoder(largeWindow: Boolean): Ptr[Byte] =
    val state = brotli.BrotliDecoderCreateInstance(nullPtr, nullPtr, nullPtr)
    if state != null && largeWindow then // scalafix:ok DisableSyntax.null
      val _ = brotli.BrotliDecoderSetParameter(state, BConst.DecoderParamLargeWindow, 1.toUInt)
    state

  def isNull(state: Ptr[Byte]): Boolean = state == null // scalafix:ok DisableSyntax.null

  def scratch(): Ptr[Byte] = scala.scalanative.libc.stdlib.malloc(BConst.ScratchSize.toCSize)

  def free(pointer: Ptr[Byte]): Unit = scala.scalanative.libc.stdlib.free(pointer)

  // Compress `input` under `op` (process per chunk, flush at a boundary, finish to close), draining
  // until the input is consumed and the encoder holds no more output.
  def encode(state: Ptr[Byte], scratchPtr: Ptr[Byte], input: Slice, op: Int): Chunk[Byte] =
    val availableIn = stackalloc[CSize](); availableIn(0) = input.length.toCSize
    val nextIn = stackalloc[Ptr[Byte]](); nextIn(0) = input.unsafePtr
    val availableOut = stackalloc[CSize]()
    val nextOut = stackalloc[Ptr[Byte]]()
    val totalOut = stackalloc[CSize]()
    encodeLoop(state, scratchPtr, op, availableIn, nextIn, availableOut, nextOut, totalOut, Chunk.empty[Byte])

  @annotation.tailrec
  private def encodeLoop(
    state: Ptr[Byte],
    scratchPtr: Ptr[Byte],
    op: Int,
    availableIn: Ptr[CSize],
    nextIn: Ptr[Ptr[Byte]],
    availableOut: Ptr[CSize],
    nextOut: Ptr[Ptr[Byte]],
    totalOut: Ptr[CSize],
    acc: Chunk[Byte]
  ): Chunk[Byte] =
    availableOut(0) = BConst.ScratchSize.toCSize
    nextOut(0) = scratchPtr
    val ok = brotli.BrotliEncoderCompressStream(state, op, availableIn, nextIn, availableOut, nextOut, totalOut)
    val produced = BConst.ScratchSize - availableOut(0).toInt
    val next = if produced > 0 then acc ++ copyOut(scratchPtr, produced) else acc
    if ok != 0 && (availableIn(0).toInt > 0 || brotli.BrotliEncoderHasMoreOutput(state) != 0) then
      encodeLoop(state, scratchPtr, op, availableIn, nextIn, availableOut, nextOut, totalOut, next)
    else next
  end encodeLoop

  // Decompress `input`, enforcing `budget` on the cumulative output (`total0` carried across calls).
  def decode(
    state: Ptr[Byte],
    scratchPtr: Ptr[Byte],
    input: Slice,
    budget: Budget,
    total0: Long
  ): Either[CompressError, DecodeStep] =
    val availableIn = stackalloc[CSize](); availableIn(0) = input.length.toCSize
    val nextIn = stackalloc[Ptr[Byte]](); nextIn(0) = input.unsafePtr
    val availableOut = stackalloc[CSize]()
    val nextOut = stackalloc[Ptr[Byte]]()
    val totalOut = stackalloc[CSize]()
    decodeLoop(state, scratchPtr, budget, availableIn, nextIn, availableOut, nextOut, totalOut, total0, Chunk.empty[Byte])

  @annotation.tailrec
  private def decodeLoop(
    state: Ptr[Byte],
    scratchPtr: Ptr[Byte],
    budget: Budget,
    availableIn: Ptr[CSize],
    nextIn: Ptr[Ptr[Byte]],
    availableOut: Ptr[CSize],
    nextOut: Ptr[Ptr[Byte]],
    totalOut: Ptr[CSize],
    total: Long,
    acc: Chunk[Byte]
  ): Either[CompressError, DecodeStep] =
    availableOut(0) = BConst.ScratchSize.toCSize
    nextOut(0) = scratchPtr
    val result = brotli.BrotliDecoderDecompressStream(state, availableIn, nextIn, availableOut, nextOut, totalOut)
    val produced = BConst.ScratchSize - availableOut(0).toInt
    val next = if produced > 0 then acc ++ copyOut(scratchPtr, produced) else acc
    val total2 = total + produced
    if budget.exceededBy(total2) then Left(CompressError.BudgetExceeded(budget.limit))
    else
      result match
        case BConst.ResultSuccess => Right(DecodeStep(next, total2, true))
        case BConst.ResultNeedsMoreOutput =>
          decodeLoop(state, scratchPtr, budget, availableIn, nextIn, availableOut, nextOut, totalOut, total2, next)
        case BConst.ResultNeedsMoreInput => Right(DecodeStep(next, total2, false))
        case _ => Left(CompressError.Malformed(message(state)))
  end decodeLoop

  private def copyOut(scratchPtr: Ptr[Byte], length: Int): Chunk[Byte] =
    val array = new Array[Byte](length)
    val _ = string.memcpy(array.atUnsafe(0), scratchPtr, length.toCSize)
    Chunk.array(array)

  def chunkSlice(chunk: Chunk[Byte]): Slice =
    val arraySlice = chunk.toArraySlice
    Slice.of(arraySlice.values, arraySlice.offset, arraySlice.length)

  private def message(state: Ptr[Byte]): String =
    fromCString(brotli.BrotliDecoderErrorString(brotli.BrotliDecoderGetErrorCode(state)))
end Native
