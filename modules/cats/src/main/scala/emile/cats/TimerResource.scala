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

import cats.effect.IO
import cats.effect.Resource

import boilerplate.effect.*
import boilerplate.effect.Eff

import emile.EmileError
import emile.Open
import emile.Timeout
import emile.Timer

/** cats-effect Resource integration for Timer handles.
  *
  * All libuv operations are serialized through `EffAsync.onLoop` / `EffAsync.closeHandle` to
  * prevent races with the shared event loop.
  */
object TimerResource:

  /** Create a timer handle as a managed resource. */
  def make: Resource[Eff.Of[IO, EmileError], Timer[Open]] =
    Resource.make(
      acquire = EffAsync.onLoop(loop => Timer.init(loop))
    )(
      release = t => EffAsync.closeHandle(t.closeAsync)
    )

  /** Create a one-shot timer as a resource. Internal: consumers use `IO.sleep` for delays. */
  private[cats] def after(timeout: Timeout)(callback: () => Unit): Resource[Eff.Of[IO, EmileError], Timer[Open]] =
    Resource.make(
      acquire = EffAsync.onLoop(loop => Timer.after(loop, timeout)(callback))
    )(
      release = t => EffAsync.closeHandle(t.closeAsync)
    )

  /** Create a repeating timer as a resource. Internal: consumers use FS2 streams for intervals. */
  private[cats] def interval(interval: Timeout)(callback: () => Unit): Resource[Eff.Of[IO, EmileError], Timer[Open]] =
    Resource.make(
      acquire = EffAsync.onLoop(loop => Timer.interval(loop, interval)(callback))
    )(
      release = t => EffAsync.closeHandle(t.closeAsync)
    )
end TimerResource
