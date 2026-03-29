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
package emile.cats

import cats.effect.unsafe.IORuntime

import munit.CatsEffectSuite

/** Base test suite that provides an IORuntime with LibuvPollingSystem.
  *
  * This is critical for testing libuv-based code with cats-effect. The default IORuntime uses
  * cats-effect's native polling (epoll/kqueue), which doesn't integrate with libuv's event loop.
  * This suite provides a runtime where libuv IS the polling backend, ensuring callbacks work.
  *
  * All emile-cats tests should extend this class.
  */
abstract class EmileSuite extends CatsEffectSuite:

  /** Runtime with LibuvPollingSystem as the polling backend.
    *
    * This ensures that:
    *   - libuv's event loop is properly integrated with cats-effect
    *   - FileDescriptorPoller.get returns a poller that works with libuv
    *   - No SIGSEGV from two separate event loops conflicting
    */
  implicit override lazy val munitIORuntime: IORuntime =
    IORuntime
      .builder()
      .setPollingSystem(LibuvPollingSystem.default)
      .build()

end EmileSuite
