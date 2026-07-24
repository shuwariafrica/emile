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

import emile.unsafe.LibUV

/** Whether [[FS$ FS]].copy attempts a copy-on-write reflink: `None` for a plain byte copy,
  * `Attempt` to reflink where the filesystem supports it and fall back to a byte copy otherwise,
  * `Require` to reflink or fail.
  */
enum ReflinkMode derives CanEqual:
  case None, Attempt, Require

/** Options for [[FS$ FS]].copy - whether to overwrite an existing destination, and the
  * [[ReflinkMode]]. Built directly or by `copy` from another; the flag word is derived on
  * [[CopyOptions$ CopyOptions]].
  */
final case class CopyOptions(overwrite: Boolean, reflink: ReflinkMode) derives CanEqual

/** The `uv_fs_copyfile` flag derivation for [[CopyOptions]]. */
object CopyOptions:

  // The uv_fs_copyfile flag word: EXCL fails if the destination exists (so it is set when overwrite
  // is off); FICLONE attempts a reflink with a byte-copy fallback, FICLONE_FORCE reflinks or errors.
  private[emile] def flags(options: CopyOptions): Int =
    val exclusive = if options.overwrite then 0 else LibUV.UV_FS_COPYFILE_EXCL
    val reflink = options.reflink match
      case ReflinkMode.None => 0
      case ReflinkMode.Attempt => LibUV.UV_FS_COPYFILE_FICLONE
      case ReflinkMode.Require => LibUV.UV_FS_COPYFILE_FICLONE_FORCE
    exclusive | reflink
