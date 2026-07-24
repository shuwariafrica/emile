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

// The zlib-ng native (ZLIB_COMPAT=OFF) streaming surface, bound directly. `zng_`-prefixed symbols do
// not collide with the system libz that scala-native's java.util.zip links, so the vendored archive
// co-links cleanly. The struct mirrors zng_stream (zlib-ng.h): size_t fields are CSize, the two
// counters and windowBits selector drive the standard z_stream loop. See RFC 1950/1951/1952.

private[zlib] type ZngStream = CStruct14[
  Ptr[Byte], // next_in
  CUnsignedInt, // avail_in
  CSize, // total_in
  Ptr[Byte], // next_out
  CUnsignedInt, // avail_out
  CSize, // total_out
  CString, // msg
  Ptr[Byte], // state
  Ptr[Byte], // zalloc
  Ptr[Byte], // zfree
  Ptr[Byte], // opaque
  CInt, // data_type
  CUnsignedInt, // adler
  CSize // reserved
]

@extern
private[zlib] object zlibng:
  def zng_deflateInit2(strm: Ptr[ZngStream], level: CInt, method: CInt, windowBits: CInt, memLevel: CInt, strategy: CInt): CInt = extern
  def zng_deflate(strm: Ptr[ZngStream], flush: CInt): CInt = extern
  def zng_deflateReset(strm: Ptr[ZngStream]): CInt = extern
  def zng_deflateEnd(strm: Ptr[ZngStream]): CInt = extern
  def zng_inflateInit2(strm: Ptr[ZngStream], windowBits: CInt): CInt = extern
  def zng_inflate(strm: Ptr[ZngStream], flush: CInt): CInt = extern
  def zng_inflateReset(strm: Ptr[ZngStream]): CInt = extern
  def zng_inflateEnd(strm: Ptr[ZngStream]): CInt = extern

private[zlib] object ZConst:
  inline val NoFlush = 0
  inline val SyncFlush = 2
  inline val Finish = 4
  inline val Ok = 0
  inline val StreamEnd = 1
  inline val NeedDict = 2
  inline val StreamError = -2
  inline val DataError = -3
  inline val MemError = -4
  inline val BufError = -5
  inline val Deflated = 8
  inline val DefaultStrategy = 0
  // One recv-cluster-sized output scratch, reused across a session's calls; output larger than this
  // drains across several iterations.
  inline val ScratchSize = 65536
end ZConst
