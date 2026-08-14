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

import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import cats.effect.IO

// Covers [[AsyncSignal]]: a fire surfaces on the fires stream, rapid fires coalesce to a single
// pending wake-up, and a second concurrent consumer fails with
// [[EmileError.IO.ConflictingOperation]].
final class AsyncSignalSpec extends EmileSuite:

  test("fire delivers a wake-up to fires") {
    AsyncSignal.resource
      .use(signal => signal.fire *> signal.fires.head.compile.drain)
      .absolve
      .timeout(5.seconds)
  }

  test("rapid fires coalesce to a single pending wake-up") {
    AsyncSignal.resource
      .use(signal =>
        EffIO.liftF(
          for
            _ <- signal.fire.absolve *> signal.fire.absolve *> signal.fire.absolve
            _ <- signal.fires.head.compile.drain.absolve
            // the capacity-one buffer is now drained: a second wake-up must not be waiting, so the
            // three fires collapsed to one
            again <- signal.fires.head.compile.drain.absolve.timeout(300.millis).attempt
            _ <- IO(assert(again.isLeft, s"expected the coalesced buffer to be empty, got: $again"))
          yield ()
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("a second concurrent fires consumer fails fast with ConflictingOperation") {
    AsyncSignal.resource
      .use(signal =>
        EffIO.liftF(
          for
            // the first consumer holds the single-consumer slot; with no fire it blocks on take
            consuming <- signal.fires.compile.drain.absolve.start
            _ <- IO.sleep(100.millis)
            result <- signal.fires.head.compile.toList.either
            _ <- consuming.cancel
            _ <- IO(result match
                   case Left(EmileError.IO.ConflictingOperation) => ()
                   case other => fail(s"expected ConflictingOperation, got: $other"))
          yield ()
        )
      )
      .absolve
      .timeout(5.seconds)
  }

end AsyncSignalSpec
