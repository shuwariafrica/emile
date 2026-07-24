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

/** How one timestamp is to be set by [[FS$ FS]].setTimes: to an explicit [[FileTime]] (`At`), to
  * the current wall-clock time (`Now`), or left unchanged (`Omit`). The two timestamps - access and
  * modification - are chosen independently. Mapped to the `uv_fs_utime` seconds-as-double sentinels
  * on [[SetTime$ SetTime]].
  */
enum SetTime derives CanEqual:
  case At(time: FileTime)
  case Now
  case Omit

/** The `uv_fs_utime` value mapping for [[SetTime]]. */
object SetTime:

  // uv_fs_utime reads seconds-as-double: a finite value sets that time, +infinity means "now"
  // (UV_FS_UTIME_NOW), NaN means "leave unchanged" (UV_FS_UTIME_OMIT).
  private[emile] def toDouble(set: SetTime): Double = set match
    case At(time) => time.toSecondsDouble
    case Now => Double.PositiveInfinity
    case Omit => Double.NaN
