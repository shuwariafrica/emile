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

import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO

import emile.unsafe.LibUV

/** Host and process memory queries, for sizing buffers and pools against the memory actually
  * available rather than an assumption. Each is an infallible read of a live kernel figure; compose
  * them where a pool or read-buffer budget should track the host.
  */
object Memory:

  /** The process's memory budget in bytes, or `None` when unconstrained. `None` covers both libuv
    * sentinels: `0` (no limiting mechanism, or the figure is unknown) and `UINT64_MAX` (a mechanism
    * exists - a Linux cgroup, z/OS `RLIMIT_MEMLIMIT` - but no limit is set). A present limit may be
    * either side of [[total]], so treat it as the budget, not a fraction of physical RAM.
    */
  def constrained: EmIO[Nothing, Option[Long]] =
    EffIO.suspend:
      val raw = LibUV.uv_get_constrained_memory()
      if raw == 0.toULong || raw == ULong.MaxValue then None else Some(raw.toLong)

  /** Free memory still available to the process under any limit, in bytes; `0` when unknown. Equals
    * [[free]] when the process is unconstrained.
    */
  def available: EmIO[Nothing, Long] =
    EffIO.suspend(LibUV.uv_get_available_memory().toLong)

  /** Free physical RAM as the kernel reports it, in bytes; `0` when unknown. */
  def free: EmIO[Nothing, Long] =
    EffIO.suspend(LibUV.uv_get_free_memory().toLong)

  /** Total physical RAM, in bytes; `0` when unknown. */
  def total: EmIO[Nothing, Long] =
    EffIO.suspend(LibUV.uv_get_total_memory().toLong)

end Memory
