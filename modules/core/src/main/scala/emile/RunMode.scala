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

/** Run mode for the event loop.
  *
  * Controls how `Loop.run` processes events.
  */
enum RunMode derives CanEqual:
  /** Run until no more active handles/requests. Default mode. */
  case Default

  /** Poll once, blocking if no pending callbacks. */
  case Once

  /** Poll once, non-blocking. Returns immediately if no pending events. */
  case NoWait

object RunMode:
  /** Convert to libuv UV_RUN_* constant. */
  extension (mode: RunMode)
    private[emile] inline def toLibuv: Int = mode match
      case Default => 0 // UV_RUN_DEFAULT
      case Once    => 1 // UV_RUN_ONCE
      case NoWait  => 2 // UV_RUN_NOWAIT
