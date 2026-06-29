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

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.IORuntimeBuilder
import cats.effect.unsafe.PollingSystem

/** A `cats.effect` application whose runtime is driven by the libuv polling system. Extend this and
  * implement [[runEff]]; override [[loopConfig]] to tune the per-worker loops. For an application
  * that ignores its process arguments, extend [[EmileIOApp.Simple]] instead.
  */
trait EmileIOApp extends IOApp:

  /** The libuv loop tuning applied to every worker. Override to change it. */
  def loopConfig: LoopConfig = LoopConfig.default

  /** The application body, in the typed-error effect. */
  def runEff(args: List[String]): EmIO[EmileError, ExitCode]

  final override def run(args: List[String]): IO[ExitCode] = runEff(args).absolve

  final override protected def pollingSystem: PollingSystem = LibuvPollingSystem(loopConfig)

/** Companion of [[EmileIOApp]]; holds the argument-free [[EmileIOApp.Simple Simple]] variant. */
object EmileIOApp:

  /** An [[EmileIOApp]] for an application that ignores its process arguments and always completes
    * with [[cats.effect.ExitCode.Success]].
    */
  trait Simple extends IOApp.Simple:

    /** The libuv loop tuning applied to every worker. Override to change it. */
    def loopConfig: LoopConfig = LoopConfig.default

    /** The application body, in the typed-error effect. */
    def runEff: EmIO[EmileError, Unit]

    final override def run: IO[Unit] = runEff.absolve

    final override protected def pollingSystem: PollingSystem = LibuvPollingSystem(loopConfig)

end EmileIOApp

/** Builds and runs a `cats.effect` [[cats.effect.unsafe.IORuntime IORuntime]] on the libuv polling
  * system, for code that does not extend [[EmileIOApp]].
  */
object Emile:

  /** A `Resource` yielding an `IORuntime` on the libuv polling system with the default
    * [[LoopConfig]]; the runtime is shut down when the resource is released. Intended for code
    * driving emile from a foreign entry point that owns the process lifecycle.
    */
  def runtime: Resource[IO, IORuntime] = runtime(LoopConfig.default)

  /** A `Resource` yielding an `IORuntime` on the libuv polling system tuned by `config`; the
    * runtime is shut down when the resource is released.
    */
  def runtime(config: LoopConfig): Resource[IO, IORuntime] =
    Resource.make(IO.delay(unsafeRuntime(config)))(rt => IO.delay(rt.shutdown()))

  /** Builds a libuv `IORuntime` whose shutdown the caller owns. The [[runtime]] resources and the
    * synchronous [[runEff]] runners are layered on this.
    */
  private[emile] def unsafeRuntime(config: LoopConfig): IORuntime =
    IORuntimeBuilder().setPollingSystem(LibuvPollingSystem(config)).build()

  /** Run a typed-error effect to its value on a fresh libuv `IORuntime` with the default
    * [[LoopConfig]], shutting the runtime down afterwards.
    */
  def runEff[A](eff: EmIO[EmileError, A]): A = runEff(LoopConfig.default)(eff)

  /** Run a typed-error effect to its value on a fresh libuv `IORuntime` tuned by `config`, shutting
    * the runtime down afterwards.
    */
  def runEff[A](config: LoopConfig)(eff: EmIO[EmileError, A]): A =
    val rt = unsafeRuntime(config)
    try eff.absolve.unsafeRunSync()(using rt)
    finally rt.shutdown()

end Emile
