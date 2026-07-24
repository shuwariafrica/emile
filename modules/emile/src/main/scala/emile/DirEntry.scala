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

/** The kind of a directory entry as the platform reports it. `Unknown` is a genuine outcome on
  * filesystems that do not carry the type in the directory record - `stat` the entry to resolve it.
  */
enum DirEntryKind derives CanEqual:
  case Unknown, File, Directory, Symlink, Fifo, Socket, CharacterDevice, BlockDevice

/** One entry from [[FS$ FS]].list - the entry's `name` relative to the listed directory, and its
  * [[DirEntryKind]]. Constructed through [[DirEntry$ DirEntry]].
  */
final case class DirEntry(name: String, kind: DirEntryKind) derives CanEqual

/** The `uv_dirent_type_t` mapping for [[DirEntry]]. */
object DirEntry:

  // uv_dirent_type_t ordinals: 0 UNKNOWN, 1 FILE, 2 DIR, 3 LINK, 4 FIFO, 5 SOCKET, 6 CHAR, 7 BLOCK.
  private[emile] def kindOf(direntType: Int): DirEntryKind = direntType match
    case 1 => DirEntryKind.File
    case 2 => DirEntryKind.Directory
    case 3 => DirEntryKind.Symlink
    case 4 => DirEntryKind.Fifo
    case 5 => DirEntryKind.Socket
    case 6 => DirEntryKind.CharacterDevice
    case 7 => DirEntryKind.BlockDevice
    case _ => DirEntryKind.Unknown
