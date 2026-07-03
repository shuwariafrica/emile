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

/** Options for an [[IPCServer]], applied during [[IPC.bind]]. Construct from
  * [[IPCOptions$ IPCOptions]].default with `copy`.
  *
  * @param mode the socket file's access mode, set before the server starts listening so it is never
  *   briefly reachable at a wider mode; `None` leaves the mode the process umask yields at bind.
  *   Meaningful only for a filesystem-path server - an abstract-namespace or autobind socket has no
  *   file - so a mode on such an address is rejected.
  * @param listenBacklog the `listen(2)` backlog
  */
final case class IPCOptions(mode: Option[IPCMode], listenBacklog: Int) derives CanEqual

/** Presets for [[IPCOptions]]. */
object IPCOptions:

  /** No mode override - the umask default at bind - and a listen backlog of 128. */
  val default: IPCOptions = IPCOptions(mode = None, listenBacklog = 128)
