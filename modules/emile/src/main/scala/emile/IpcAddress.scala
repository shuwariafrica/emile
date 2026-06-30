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

/** The address of an [[Ipc$ Ipc]] (Unix-domain / named-pipe) endpoint - the kind-indexed address an
  * [[IpcSocket]] / [[IpcServer]] carries, as a filesystem path, a Linux abstract name, or autobind.
  */
enum IpcAddress derives CanEqual:

  /** A filesystem entry - a Unix-domain socket path on Unix, a named pipe on Windows. */
  case Path(value: String)

  /** A Linux abstract-namespace name - bound and connected with no filesystem entry, so it leaves
    * no residue and needs no path permissions. Linux-only.
    */
  case Abstract(name: String)

  /** Bind a kernel-assigned abstract name, reported back as [[Abstract]] once bound. For a private
    * listener whose name need not be agreed in advance. Linux-only, and meaningful only for a bind.
    */
  case Autobind
