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

// The brotli streaming encoder/decoder, bound directly (encode.h / decode.h). Both drive a pull/push
// cursor model: available_in/next_in and available_out/next_out advance in place, and total_out
// accumulates. State objects are opaque; the default allocator is used (null alloc/free/opaque).

@extern
private[brotli] object brotli:
  def BrotliEncoderCreateInstance(alloc: Ptr[Byte], free: Ptr[Byte], opaque: Ptr[Byte]): Ptr[Byte] = extern
  def BrotliEncoderSetParameter(state: Ptr[Byte], param: CInt, value: CUnsignedInt): CInt = extern
  def BrotliEncoderCompressStream(
    state: Ptr[Byte],
    op: CInt,
    availableIn: Ptr[CSize],
    nextIn: Ptr[Ptr[Byte]],
    availableOut: Ptr[CSize],
    nextOut: Ptr[Ptr[Byte]],
    totalOut: Ptr[CSize]
  ): CInt = extern
  def BrotliEncoderHasMoreOutput(state: Ptr[Byte]): CInt = extern
  def BrotliEncoderDestroyInstance(state: Ptr[Byte]): Unit = extern
  def BrotliDecoderCreateInstance(alloc: Ptr[Byte], free: Ptr[Byte], opaque: Ptr[Byte]): Ptr[Byte] = extern
  def BrotliDecoderSetParameter(state: Ptr[Byte], param: CInt, value: CUnsignedInt): CInt = extern
  def BrotliDecoderDecompressStream(
    state: Ptr[Byte],
    availableIn: Ptr[CSize],
    nextIn: Ptr[Ptr[Byte]],
    availableOut: Ptr[CSize],
    nextOut: Ptr[Ptr[Byte]],
    totalOut: Ptr[CSize]
  ): CInt = extern
  def BrotliDecoderDestroyInstance(state: Ptr[Byte]): Unit = extern
  def BrotliDecoderGetErrorCode(state: Ptr[Byte]): CInt = extern
  def BrotliDecoderErrorString(code: CInt): CString = extern
end brotli

private[brotli] object BConst:
  inline val OpProcess = 0
  inline val OpFlush = 1
  inline val OpFinish = 2
  inline val ParamMode = 0
  inline val ParamQuality = 1
  inline val ParamLgWin = 2
  inline val DecoderParamLargeWindow = 1
  inline val ResultError = 0
  inline val ResultSuccess = 1
  inline val ResultNeedsMoreInput = 2
  inline val ResultNeedsMoreOutput = 3
  inline val ScratchSize = 65536
