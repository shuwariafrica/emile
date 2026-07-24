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

// The zstd streaming surface, bound directly (zstd.h). Compression and decompression each drive an
// in/out ZSTD_Buffer whose `pos` advances in place; the context objects are opaque. Single-threaded:
// nbWorkers stays 0 (the archive is built without ZSTD_MULTITHREAD).

private[zstd] type ZstdBuffer = CStruct3[Ptr[Byte], CSize, CSize] // {ptr, size, pos}

@extern
private[zstd] object zstd:
  def ZSTD_createCStream(): Ptr[Byte] = extern
  def ZSTD_freeCStream(zcs: Ptr[Byte]): CSize = extern
  def ZSTD_CCtx_setParameter(cctx: Ptr[Byte], param: CInt, value: CInt): CSize = extern
  def ZSTD_CCtx_reset(cctx: Ptr[Byte], directive: CInt): CSize = extern
  def ZSTD_compressStream2(cctx: Ptr[Byte], output: Ptr[ZstdBuffer], input: Ptr[ZstdBuffer], endOp: CInt): CSize = extern
  def ZSTD_createDStream(): Ptr[Byte] = extern
  def ZSTD_freeDStream(zds: Ptr[Byte]): CSize = extern
  def ZSTD_DCtx_setParameter(dctx: Ptr[Byte], param: CInt, value: CInt): CSize = extern
  def ZSTD_DCtx_reset(dctx: Ptr[Byte], directive: CInt): CSize = extern
  def ZSTD_decompressStream(zds: Ptr[Byte], output: Ptr[ZstdBuffer], input: Ptr[ZstdBuffer]): CSize = extern
  def ZSTD_isError(code: CSize): CUnsignedInt = extern
  def ZSTD_getErrorName(code: CSize): CString = extern

private[zstd] object ZConst:
  inline val ParamCompressionLevel = 100 // ZSTD_c_compressionLevel
  inline val ParamWindowLogMax = 100 // ZSTD_d_windowLogMax (distinct DCtx parameter space)
  inline val EContinue = 0
  inline val EFlush = 1
  inline val EEnd = 2
  inline val ResetSessionOnly = 1
  inline val ScratchSize = 65536
