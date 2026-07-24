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

import scala.scalanative.libc.string
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import fs2.Chunk

import emile.compress.Budget
import emile.compress.CompressError

// One decode iteration's yield: the output produced, the running output total (for the budget), and
// whether ZSTD_decompressStream reported a complete frame (return 0).
final private[zstd] case class DecodeStep(output: Chunk[Byte], total: Long, finished: Boolean)

// All zstd FFI: context creation, the two ZSTD_Buffer drain loops, and the buffer conversions.
private[zstd] object Native:

  def createEncoder(level: Int): Ptr[Byte] =
    val cctx = zstd.ZSTD_createCStream()
    if cctx != null then // scalafix:ok DisableSyntax.null
      val _ = zstd.ZSTD_CCtx_setParameter(cctx, ZConst.ParamCompressionLevel, level)
    cctx

  def createDecoder(windowLogMax: Int): Ptr[Byte] =
    val dctx = zstd.ZSTD_createDStream()
    if dctx != null then // scalafix:ok DisableSyntax.null
      val _ = zstd.ZSTD_DCtx_setParameter(dctx, ZConst.ParamWindowLogMax, windowLogMax)
    dctx

  def isNull(context: Ptr[Byte]): Boolean = context == null // scalafix:ok DisableSyntax.null

  def scratch(): Ptr[Byte] = scala.scalanative.libc.stdlib.malloc(ZConst.ScratchSize.toCSize)

  def free(pointer: Ptr[Byte]): Unit = scala.scalanative.libc.stdlib.free(pointer)

  // Compress `input` under `endOp` (ZSTD_e_continue per chunk, ZSTD_e_flush at a boundary,
  // ZSTD_e_end to close the frame), draining the output each call.
  def encode(cctx: Ptr[Byte], scratchPtr: Ptr[Byte], input: Slice, endOp: Int): Chunk[Byte] =
    val in = stackalloc[ZstdBuffer]()
    in._1 = input.unsafePtr; in._2 = input.length.toCSize; in._3 = 0.toCSize
    val out = stackalloc[ZstdBuffer]()
    encodeLoop(cctx, scratchPtr, in, out, endOp, Chunk.empty[Byte])

  @annotation.tailrec
  private def encodeLoop(
    cctx: Ptr[Byte],
    scratchPtr: Ptr[Byte],
    in: Ptr[ZstdBuffer],
    out: Ptr[ZstdBuffer],
    endOp: Int,
    acc: Chunk[Byte]): Chunk[Byte] =
    out._1 = scratchPtr; out._2 = ZConst.ScratchSize.toCSize; out._3 = 0.toCSize
    val code = zstd.ZSTD_compressStream2(cctx, out, in, endOp)
    val produced = out._3.toInt
    val next = if produced > 0 then acc ++ copyOut(scratchPtr, produced) else acc
    // e_continue finishes when the input is consumed; e_flush/e_end finish when the hint returns 0.
    // Stop on an error (an infallible-path defect) rather than spin.
    val done =
      if zstd.ZSTD_isError(code).toInt != 0 then true
      else if endOp == ZConst.EContinue then in._3.toInt >= in._2.toInt
      else code.toLong == 0L
    if done then next else encodeLoop(cctx, scratchPtr, in, out, endOp, next)
  end encodeLoop

  // Decompress `input`, enforcing `budget` on the cumulative output (`total0` carried across calls).
  // A window-too-large refusal (windowLogMax) surfaces here as ZSTD_isError, mapped to Malformed.
  def decode(dctx: Ptr[Byte], scratchPtr: Ptr[Byte], input: Slice, budget: Budget, total0: Long): Either[CompressError, DecodeStep] =
    val in = stackalloc[ZstdBuffer]()
    in._1 = input.unsafePtr; in._2 = input.length.toCSize; in._3 = 0.toCSize
    val out = stackalloc[ZstdBuffer]()
    decodeLoop(dctx, scratchPtr, in, out, budget, total0, Chunk.empty[Byte])

  @annotation.tailrec
  private def decodeLoop(
    dctx: Ptr[Byte],
    scratchPtr: Ptr[Byte],
    in: Ptr[ZstdBuffer],
    out: Ptr[ZstdBuffer],
    budget: Budget,
    total: Long,
    acc: Chunk[Byte]
  ): Either[CompressError, DecodeStep] =
    out._1 = scratchPtr; out._2 = ZConst.ScratchSize.toCSize; out._3 = 0.toCSize
    val code = zstd.ZSTD_decompressStream(dctx, out, in)
    val produced = out._3.toInt
    val next = if produced > 0 then acc ++ copyOut(scratchPtr, produced) else acc
    val total2 = total + produced
    if budget.exceededBy(total2) then Left(CompressError.BudgetExceeded(budget.limit))
    else if zstd.ZSTD_isError(code).toInt != 0 then Left(CompressError.Malformed(fromCString(zstd.ZSTD_getErrorName(code))))
    else if code.toLong == 0L then Right(DecodeStep(next, total2, true))
    else if in._3.toInt < in._2.toInt || out._3.toInt == ZConst.ScratchSize then decodeLoop(dctx, scratchPtr, in, out, budget, total2, next)
    else Right(DecodeStep(next, total2, false))
  end decodeLoop

  private def copyOut(scratchPtr: Ptr[Byte], length: Int): Chunk[Byte] =
    val array = new Array[Byte](length)
    val _ = string.memcpy(array.atUnsafe(0), scratchPtr, length.toCSize)
    Chunk.array(array)

  def chunkSlice(chunk: Chunk[Byte]): Slice =
    val arraySlice = chunk.toArraySlice
    Slice.of(arraySlice.values, arraySlice.offset, arraySlice.length)
end Native
