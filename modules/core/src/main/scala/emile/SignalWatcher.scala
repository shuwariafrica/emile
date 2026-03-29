/*
 * Copyright 2025, 2026 Ali Rashid.
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

/** Signal watcher facade using libuv's native signal handling.
  *
  * This provides a simplified registration model for signal watching that wraps the
  * [[SignalHandle]] API. Internally, it maintains per-signal handles for the
  * registration/unregistration pattern.
  *
  * ==Cross-Platform Support==
  *
  * This uses libuv's native `uv_signal_*` API which provides cross-platform signal handling:
  *
  *   - '''Unix/Linux/macOS''': Full POSIX signal support (except SIGKILL/SIGSTOP which cannot be
  *     caught)
  *   - '''Windows''': Supports SIGINT, SIGBREAK, SIGHUP, SIGWINCH. Other signals can have watchers
  *     created but will never fire.
  *
  * Use [[Signal.isSupported]] to check if a signal is deliverable on the current platform.
  *
  * ==Thread Safety==
  *
  * All callbacks run on the libuv event loop thread during normal loop iteration. The signal
  * handler itself is async-signal-safe internally in libuv.
  *
  * ==Lifecycle==
  *
  * Unlike the lower-level [[SignalHandle]], this uses a registration model:
  *   - `watch` registers a signal and returns the handle
  *   - `unwatch` stops watching and cleans up
  *
  * Only one watcher can be active per (loop, signal) pair at a time.
  *
  * @see [[SignalHandle]] for the lower-level handle-based API
  * @see [[Signal]] for signal number constants and platform support checks
  */
object SignalWatcher:

  /** Start watching for a signal.
    *
    * The callback is invoked during normal loop iteration when the signal arrives.
    *
    * @param loop The event loop
    * @param signum The signal number to watch
    * @param callback The callback to invoke when the signal arrives
    * @return Either an error or the signal handle (for later closing)
    */
  def watch(loop: Loop)(signum: Int)(callback: () => Unit): Either[EmileError, SignalHandle[Open]] =
    SignalHandle.watch(loop, signum)(callback)

  /** Start watching for a signal, one-shot mode.
    *
    * The watcher is automatically stopped after the first signal arrives. The callback is still
    * invoked for that single signal.
    *
    * @param loop The event loop
    * @param signum The signal number to watch
    * @param callback The callback to invoke when the signal arrives
    * @return Either an error or the signal handle
    */
  def watchOnce(loop: Loop)(signum: Int)(callback: () => Unit): Either[EmileError, SignalHandle[Open]] =
    SignalHandle.once(loop, signum)(callback)

  /** Check if a signal is supported on the current platform.
    *
    * @param signum The signal number to check
    * @return true if signals of this type will be delivered
    */
  def isSupported(signum: Int): Boolean =
    Signal.isSupported(signum)

end SignalWatcher
