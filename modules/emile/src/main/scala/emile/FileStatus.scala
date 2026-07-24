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

import scala.scalanative.posix.sys.stat
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*

import emile.unsafe.LibUV

/** A filesystem timestamp as whole seconds since the Unix epoch plus a nanosecond remainder,
  * ordered and comparable without a `java.time` dependency. Represented as the `(seconds, nanos)`
  * pair; construct and read through [[FileTime$ FileTime]].
  */
opaque type FileTime = (Long, Long)

/** Factory and accessors for [[FileTime]]. */
object FileTime:

  /** A time `seconds` past the epoch, with an additional `nanos` (`0 .. 999999999`). */
  def apply(seconds: Long, nanos: Long): FileTime = (seconds, nanos)

  given CanEqual[FileTime, FileTime] = CanEqual.derived

  given Ordering[FileTime] = Ordering.by(t => (t._1, t._2))

  extension (time: FileTime)
    /** Whole seconds since the Unix epoch. */
    def seconds: Long = time._1

    /** The nanosecond remainder within the second, `0 .. 999999999`. */
    def nanos: Long = time._2

    // Seconds-as-double, the form uv_fs_utime / uv_fs_futime take.
    private[emile] def toSecondsDouble: Double = time._1.toDouble + time._2.toDouble / 1e9

end FileTime

/** A file's metadata - the full `stat(2)` record, with times as [[FileTime]]. Produced by
  * [[OpenFile$ OpenFile]].stat, [[FS$ FS]].stat, and [[FS$ FS]].lstat; type and permission queries
  * are on [[FileStatus$ FileStatus]].
  *
  * @param blockSize the preferred I/O block size, distinct from the 512-byte unit `blocks` counts
  * @param blocks the number of 512-byte blocks allocated
  * @param flags user-defined file flags, zero where the platform does not report them
  * @param gen the file generation number, zero where the platform does not report it
  * @param lastStatusChange the inode's last change time (`ctime`), not a creation time
  * @param creation the file's creation time (`birthtime`), zero where the platform does not record
  *   it
  */
final case class FileStatus(
  dev: Long,
  mode: Long,
  nlink: Long,
  uid: Long,
  gid: Long,
  rdev: Long,
  ino: Long,
  size: Long,
  blockSize: Long,
  blocks: Long,
  flags: Long,
  gen: Long,
  lastAccess: FileTime,
  lastModified: FileTime,
  lastStatusChange: FileTime,
  creation: FileTime
) derives CanEqual

/** Type and permission queries over [[FileStatus]], and the `uv_stat_t` reader. */
object FileStatus:

  extension (status: FileStatus)
    /** A regular file. */
    def isFile: Boolean = stat.S_ISREG(status.mode.toUInt) != 0

    /** A directory. */
    def isDirectory: Boolean = stat.S_ISDIR(status.mode.toUInt) != 0

    /** A symbolic link - only ever true from [[FS$ FS]].lstat, as `stat` follows the link. */
    def isSymlink: Boolean = stat.S_ISLNK(status.mode.toUInt) != 0

    /** The permission bits - the low twelve of `mode` (`rwx` triples plus set-user-id,
      * set-group-id, and the sticky bit) as an octal `Int`.
      */
    def permissions: Int = (status.mode & 0xfffL).toInt

  // Reads a completed uv_stat_t into an owned FileStatus; the timespec fields are long pairs (.atN
  // yields a Ptr to each). Called before the request is cleaned up, while the statbuf is still live.
  private[emile] def fromStat(s: Ptr[LibUV.Stat]): FileStatus =
    FileStatus(
      dev = s._1.toLong,
      mode = s._2.toLong,
      nlink = s._3.toLong,
      uid = s._4.toLong,
      gid = s._5.toLong,
      rdev = s._6.toLong,
      ino = s._7.toLong,
      size = s._8.toLong,
      blockSize = s._9.toLong,
      blocks = s._10.toLong,
      flags = s._11.toLong,
      gen = s._12.toLong,
      lastAccess = FileTime(s.at13._1.toLong, s.at13._2.toLong),
      lastModified = FileTime(s.at14._1.toLong, s.at14._2.toLong),
      lastStatusChange = FileTime(s.at15._1.toLong, s.at15._2.toLong),
      creation = FileTime(s.at16._1.toLong, s.at16._2.toLong)
    )

end FileStatus
