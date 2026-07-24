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

import scala.scalanative.posix.fcntl

/** The access and creation intent for [[OpenFile$ OpenFile]].open, as an orthogonal set of the six
  * `open(2)` behaviours rather than a raw flag word - so no invalid flag combination is
  * representable. Built from the presets on [[OpenMode$ OpenMode]] (`Read`, `Write`, `Append`,
  * `CreateNew`, `ReadWrite`) or by `copy` from one.
  *
  * @param create create the file when it does not exist (`O_CREAT`); the permission argument to
  *   `open` applies only then
  * @param append every write lands at end of file, ignoring any positioned-write offset
  *   (`O_APPEND`)
  * @param truncate discard existing contents on open (`O_TRUNC`)
  * @param exclusive fail with [[EmileError.IO.AlreadyExists]] when combined with `create` and the
  *   file already exists (`O_EXCL`)
  */
final case class OpenMode(
  read: Boolean,
  write: Boolean,
  create: Boolean,
  append: Boolean,
  truncate: Boolean,
  exclusive: Boolean
) derives CanEqual

/** Presets and the system-flag derivation for [[OpenMode]]. */
object OpenMode:

  /** Read-only; the file must already exist - the default [[OpenFile$ OpenFile]].open behaviour. */
  val Read: OpenMode = OpenMode(read = true, write = false, create = false, append = false, truncate = false, exclusive = false)

  /** Write-only, creating the file if absent and truncating it if present. */
  val Write: OpenMode = OpenMode(read = false, write = true, create = true, append = false, truncate = true, exclusive = false)

  /** Write-only, creating the file if absent and appending to it if present. */
  val Append: OpenMode = OpenMode(read = false, write = true, create = true, append = true, truncate = false, exclusive = false)

  /** Write-only, creating the file and failing with [[EmileError.IO.AlreadyExists]] if it exists. */
  val CreateNew: OpenMode =
    OpenMode(read = false, write = true, create = true, append = false, truncate = false, exclusive = true)

  /** Read and write an existing file, leaving its contents intact. */
  val ReadWrite: OpenMode =
    OpenMode(read = true, write = true, create = false, append = false, truncate = false, exclusive = false)

  // The system open(2) flag word for `mode`. UV_FS_O_* map to the platform O_* on Unix, so the flags
  // come from the posix layer (correct per platform) rather than fixed literals; RDONLY is 0.
  private[emile] def flags(mode: OpenMode): Int =
    val access =
      if mode.read && mode.write then fcntl.O_RDWR
      else if mode.write then fcntl.O_WRONLY
      else fcntl.O_RDONLY
    access |
      (if mode.create then fcntl.O_CREAT else 0) |
      (if mode.append then fcntl.O_APPEND else 0) |
      (if mode.truncate then fcntl.O_TRUNC else 0) |
      (if mode.exclusive then fcntl.O_EXCL else 0)

end OpenMode
