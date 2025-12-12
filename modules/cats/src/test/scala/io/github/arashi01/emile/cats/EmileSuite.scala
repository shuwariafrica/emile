/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.unsafe.IORuntime
import munit.CatsEffectSuite

/**
 * Base test suite that provides an IORuntime with LibuvPollingSystem.
 *
 * This is critical for testing libuv-based code with cats-effect.
 * The default IORuntime uses cats-effect's native polling (epoll/kqueue),
 * which doesn't integrate with libuv's event loop. This suite provides
 * a runtime where libuv IS the polling backend, ensuring callbacks work.
 *
 * All emile-cats tests should extend this class.
 */
abstract class EmileSuite extends CatsEffectSuite:

  /**
   * Runtime with LibuvPollingSystem as the polling backend.
   *
   * This ensures that:
   * - libuv's event loop is properly integrated with cats-effect
   * - FileDescriptorPoller.get returns a poller that works with libuv
   * - No SIGSEGV from two separate event loops conflicting
   */
  override implicit lazy val munitIORuntime: IORuntime =
    IORuntime
      .builder()
      .setPollingSystem(LibuvPollingSystem)
      .build()

end EmileSuite
