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

/** Tuning applied to each libuv loop emile runs - one loop per cats-effect worker. Instances are
  * built from the presets and `copy` on [[LoopConfig$ LoopConfig]].
  *
  * @param blockProfilerSignal block `SIGPROF` while the loop polls (libuv `UV_LOOP_BLOCK_SIGNAL`),
  *   suppressing sampling-profiler wake-ups; libuv blocks `SIGPROF` only, so this is the whole of
  *   the choice
  * @param useIoUringSqpoll register the loop's io_uring with kernel-side submission-queue polling
  *   (libuv `UV_LOOP_USE_IO_URING_SQPOLL`)
  */
final case class LoopConfig(
  blockProfilerSignal: Boolean,
  useIoUringSqpoll: Boolean
) derives CanEqual

/** Presets for [[LoopConfig]]. */
object LoopConfig:

  /** Conservative defaults: `SIGPROF` not blocked, and io_uring SQPOLL off - SQPOLL needs
    * `CAP_SYS_NICE` on older kernels, so it is an explicit opt-in via `copy`.
    */
  val default: LoopConfig = LoopConfig(blockProfilerSignal = false, useIoUringSqpoll = false)

  /** Blocks `SIGPROF` on every loop thread so a sampling profiler can drive the process. */
  val profilerProfile: LoopConfig = default.copy(blockProfilerSignal = true)
