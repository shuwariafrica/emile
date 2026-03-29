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

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.Resource

import boilerplate.effect.*
import boilerplate.effect.Eff

import emile.EmileError
import emile.Loop
import emile.RunMode
import emile.cats.LibuvPollingSystem.LoopAccess

/** Direct access to the shared libuv event loop.
  *
  * Most consumers do not need this - the resource factories (`TimerResource`, `TcpResource`, etc.)
  * and `EffAsync.onLoop` acquire the loop internally with proper serialisation.
  *
  * Use `integrated` only when you need the raw `Loop` pointer for advanced operations (e.g., loop
  * metrics, custom handle types).
  */
object EmileLoop:

  /** Get the shared libuv loop from the runtime.
    *
    * The loop is NOT closed when the resource is released.
    */
  val integrated: Resource[Eff.Of[IO, EmileError], Loop] =
    Resource.eval(LoopAccess.get).flatMap(_.loop)

  /** Yield to the scheduler and guarantee at least one libuv poll tick.
    *
    * Use this when you need pending libuv events (callbacks, timer firings, signal deliveries) to
    * be processed before the current fibre continues. The standard `IO.cede` does NOT guarantee a
    * poll tick because cats-effect's `WorkerThread.reschedule` uses a `cedeBypass` optimisation
    * that stores the ceded fibre directly and re-executes it without entering the polling phase.
    *
    * The correct upstream fix is to guard `cedeBypass` with `!system.needsPoll(_poller)` in
    * `WorkerThread.reschedule` so the worker enters the polling phase when the polling system has
    * pending events. This combinator should be removed when cats-effect incorporates that fix.
    *
    * @see
    *   [[https://github.com/typelevel/cats-effect/blob/series/3.x/core/jvm-native/src/main/scala/cats/effect/unsafe/WorkerThread.scala WorkerThread.reschedule]]
    */
  val tick: IO[Unit] =
    // Workaround: IO.sleep engages the cats-effect scheduler timer which
    // calls poll(nanos) on the polling system, triggering uv_run. This is
    // needed because IO.cede bypasses polling due to the cedeBypass
    // optimisation in WorkerThread.reschedule. Remove when upstream adds
    // a needsPoll guard to the cedeBypass path.
    IO.sleep(1.millis)

  extension (loop: Loop)
    inline def runOnce: Eff[IO, EmileError, Boolean] =
      loop.run(RunMode.Once).eff[IO]

    def runUntilComplete: Eff[IO, EmileError, Unit] =
      def drain: Eff[IO, EmileError, Unit] =
        loop.runOnce.flatMap { alive =>
          if alive then Eff.liftF(IO.cede) *> drain else Eff.succeed[IO, EmileError, Unit](())
        }
      drain

    inline def runNoWait: Eff[IO, EmileError, Boolean] =
      loop.run(RunMode.NoWait).eff[IO]
  end extension

end EmileLoop
