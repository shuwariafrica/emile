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

import boilerplate.effect.EffIO
import fs2.Stream

/** Covers the error-channel widening helpers in `syntax`: an [[EmStream]] widens cast-free through
  * `widenS`, an [[EmPipe]] widens by phantom reinterpretation through `widen`, and both preserve
  * the success and typed-error values they carry. The `EmIO.Of` partial application is exercised as
  * the stream effect throughout.
  */
final class SyntaxSpec extends EmileSuite:

  test("widenS widens an EmStream's error channel, preserving values and a typed error") {
    val values: EmStream[EmileError.Io, Int] = Stream(1, 2, 3).covary[EmIO.Of[EmileError.Io]]
    val failed: EmStream[EmileError.Io, Int] = Stream.eval(EffIO.fail(EmileError.Io.AlreadyClosed))
    for
      ok <- values.widenS[EmileError].compile.toList.either
      err <- failed.widenS[EmileError].compile.toList.either
    yield
      assertEquals(ok, Right(List(1, 2, 3)): Either[EmileError, List[Int]])
      assertEquals(err, Left(EmileError.Io.AlreadyClosed): Either[EmileError, List[Int]])
  }

  test("widen reinterprets an EmPipe at a wider error and preserves its behaviour") {
    val doubling: EmPipe[EmileError.Io, Int, Int] = _.map(_ * 2)
    val widened: EmPipe[EmileError, Int, Int] = doubling.widen[EmileError]
    Stream(1, 2, 3)
      .covary[EmIO.Of[EmileError]]
      .through(widened)
      .compile
      .toList
      .either
      .map(result => assertEquals(result, Right(List(2, 4, 6)): Either[EmileError, List[Int]]))
  }
end SyntaxSpec
