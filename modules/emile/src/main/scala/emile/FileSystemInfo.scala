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
package emile

import scala.scalanative.unsafe.Ptr

import emile.unsafe.LibUV

/** Capacity and usage of the filesystem holding a path - the `statfs(2)` record from
  * [[FS$ FS]].statfs. Any field the underlying operating system does not report is zero;
  * `blocksAvailable` counts blocks free to an unprivileged user, `blocksFree` all free blocks.
  * Produced through [[FileSystemInfo$ FileSystemInfo]].
  *
  * @param fsType the filesystem-type magic number, zero on platforms that do not report it
  * @param fragmentSize the fundamental block size, which may differ from `blockSize`
  */
final case class FileSystemInfo(
  fsType: Long,
  blockSize: Long,
  blocks: Long,
  blocksFree: Long,
  blocksAvailable: Long,
  files: Long,
  filesFree: Long,
  fragmentSize: Long
) derives CanEqual

/** The `uv_statfs_t` reader for [[FileSystemInfo]]. */
object FileSystemInfo:

  // Reads the leading eight uint64 fields of the malloc'd uv_statfs_t at req->ptr; called before the
  // request cleanup that frees it. The f_spare[3] tail is not read.
  private[emile] def fromStatfs(s: Ptr[LibUV.Statfs]): FileSystemInfo =
    FileSystemInfo(
      fsType = s._1.toLong,
      blockSize = s._2.toLong,
      blocks = s._3.toLong,
      blocksFree = s._4.toLong,
      blocksAvailable = s._5.toLong,
      files = s._6.toLong,
      filesFree = s._7.toLong,
      fragmentSize = s._8.toLong
    )

end FileSystemInfo
