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
package emile.unsafe

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.Ptr

// raw malloc/realloc/free buffer: var pointer and capacity, null-on-OOM checks, OutOfMemoryError throw.
// scalafix:off DisableSyntax
/** A native byte buffer that grows on demand. */
final private[emile] class ResizableBuffer private (private var buffer: Ptr[Byte], private var capacity: Int):

  /** A buffer of at least `size` bytes; a pointer from an earlier `ensure` is invalidated when a
    * later call grows the buffer.
    */
  def ensure(size: Int): Ptr[Byte] =
    if size > capacity then
      val grown = stdlib.realloc(buffer, size)
      if grown == null then throw new OutOfMemoryError("emile: native buffer reallocation failed")
      buffer = grown
      capacity = size
    buffer

  def free(): Unit = stdlib.free(buffer)

object ResizableBuffer:
  def apply(initialSize: Int): ResizableBuffer =
    val buffer = stdlib.malloc(initialSize)
    if buffer == null then throw new OutOfMemoryError("emile: native buffer allocation failed")
    new ResizableBuffer(buffer, initialSize)
// scalafix:on DisableSyntax
