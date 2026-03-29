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
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.unsafe.PollingSystem

import boilerplate.effect.Eff

import emile.EmileError
import emile.Loop
import emile.LoopConfig

/** IOApp trait that uses libuv as the polling backend.
  *
  * {{{
  * object MyServer extends EmileIOApp:
  *   def run(args: List[String]): IO[ExitCode] =
  *     TcpResource.bind(address).use { tcp =>
  *       // handle connections
  *     }.rethrow
  * }}}
  *
  * Resource factories (`TimerResource`, `TcpResource`, etc.) acquire the loop internally — no
  * explicit loop plumbing needed. Use `withLoop` only for advanced operations that need the raw
  * `Loop` pointer.
  */
trait EmileIOApp extends IOApp:
  private lazy val emilePollingSystem: PollingSystem =
    LibuvPollingSystem(loopConfig)

  /** Override to customize the libuv loop configuration. */
  def loopConfig: LoopConfig = LoopConfig.empty

  override protected def pollingSystem: PollingSystem =
    emilePollingSystem

object EmileIOApp:
  /** Execute a callback with access to the shared libuv loop.
    *
    * Most consumers do not need this — resource factories acquire the loop internally. Use for
    * advanced operations only.
    */
  def withLoop[A](f: Loop => Eff[IO, EmileError, A]): Eff[IO, EmileError, A] =
    loopResource.use(f)

  /** Access the shared loop as a Resource. */
  def loopResource: Resource[Eff.Of[IO, EmileError], Loop] =
    Resource.eval(LibuvPollingSystem.LoopAccess.get).flatMap(_.loop)
end EmileIOApp
