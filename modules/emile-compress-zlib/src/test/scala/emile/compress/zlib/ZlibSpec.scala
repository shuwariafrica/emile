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
package emile.compress.zlib

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import fs2.Stream

import emile.compress.Budget
import emile.compress.CompressError
import emile.widen

// Round-trips both surfaces across all three framings and several levels, and exercises the guard
// arms, the corrupt/truncated failures, and the RFC 7692 raw-deflate substrate (sync-flush tail and
// per-message reset) that jenga's permessage-deflate rides.
final class ZlibSpec extends munit.CatsEffectSuite:

  private def sample(size: Int): Array[Byte] =
    Array.tabulate(size)(i => (32 + (i * 31 + (i / 7)) % 90).toByte)

  private def deflated(params: DeflateParams, input: Array[Byte]): IO[Vector[Byte]] =
    Stream.emits(input.toIndexedSeq).through(Deflate.compress(params)).compile.toVector.absolve

  private def inflated(params: InflateParams, input: Vector[Byte]): IO[Vector[Byte]] =
    Stream.emits(input).through(Inflate.decompress(params)).compile.toVector.absolve

  private val framings: List[(String, Framing)] =
    List("zlib" -> Framing.Zlib, "gzip" -> Framing.Gzip, "raw" -> Framing.raw(15))

  for (name, framing) <- framings; level <- List(1, 6, 9) do
    test(s"pipe round-trip: $name framing, level $level") {
      val data = sample(20000)
      deflated(DeflateParams(framing, level), data)
        .flatMap(compressed => inflated(InflateParams(framing, Budget.unlimited), compressed))
        .map(out => assertEquals(out, data.toVector))
    }

  test("session round-trip: deflate then inflate reproduce the input") {
    val data = sample(12000)
    val compress = Deflate
      .session(DeflateParams(Framing.Zlib))
      .use(session =>
        for
          body <- session.update(Slice.of(data))
          tail <- session.finish
        yield body ++ tail
      )
      .absolve
    compress
      .flatMap(compressed =>
        Inflate
          .session(InflateParams(Framing.Zlib, Budget.unlimited))
          .widen[CompressError]
          .use(session => session.update(Slice.of(compressed.toArray)))
          .absolve
      )
      .map(out => assertEquals(out.toVector, data.toVector))
  }

  test("budget: a limit equal to the output succeeds, one below aborts") {
    val data = sample(50000)
    deflated(DeflateParams.default, data).flatMap { compressed =>
      val exact = inflated(InflateParams(Framing.Zlib, Budget.bytes(data.length.toLong)), compressed)
        .map(out => assertEquals(out.length, data.length))
      val below = inflated(InflateParams(Framing.Zlib, Budget.bytes(data.length.toLong - 1)), compressed).attempt.map {
        case Left(error: CompressError.BudgetExceeded) => assertEquals(error.limit, data.length.toLong - 1)
        case other => fail(s"expected BudgetExceeded, got $other")
      }
      exact *> below
    }
  }

  test("budget: a small ceiling aborts a highly compressible stream mid-decode") {
    val data = Array.fill(1 << 20)(0.toByte)
    deflated(DeflateParams.default, data).flatMap { compressed =>
      inflated(InflateParams(Framing.Zlib, Budget.bytes(1024)), compressed).attempt.map {
        case Left(_: CompressError.BudgetExceeded) => ()
        case other => fail(s"expected BudgetExceeded, got $other")
      }
    }
  }

  test("truncated input fails with Truncated") {
    val data = sample(40000)
    deflated(DeflateParams.default, data).flatMap { compressed =>
      inflated(InflateParams(Framing.Zlib, Budget.unlimited), compressed.take(compressed.length / 2)).attempt.map {
        case Left(_: CompressError.Truncated) => ()
        case other => fail(s"expected Truncated, got $other")
      }
    }
  }

  test("corrupt input fails with Malformed") {
    val garbage = Vector[Byte](9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 120, -45, 33)
    inflated(InflateParams(Framing.Zlib, Budget.unlimited), garbage).attempt.map {
      case Left(_: CompressError.Malformed) => ()
      case other => fail(s"expected Malformed, got $other")
    }
  }

  test("raw deflate flush ends with the 00 00 ff ff empty stored block") {
    val data = sample(2000)
    Deflate
      .session(DeflateParams(Framing.raw(15)))
      .use(session =>
        for
          body <- session.update(Slice.of(data))
          tail <- session.flush
        yield body ++ tail
      )
      .absolve
      .map(block => assertEquals(block.toVector.takeRight(4), Vector[Byte](0, 0, -1, -1)))
  }

  test("reset makes each message decode independently; without reset the block does not") {
    val message = sample(3000)

    def emit(reset: Boolean): IO[Vector[Byte]] =
      Deflate
        .session(DeflateParams(Framing.raw(15)))
        .use(session =>
          for
            _ <- session.update(Slice.of(message))
            _ <- session.flush
            _ <- if reset then session.reset else EffIO.succeed(())
            _ <- session.update(Slice.of(message))
            second <- session.flush
          yield second.toVector
        )
        .absolve

    def decodeAlone(block: Vector[Byte]): IO[Either[Throwable, Vector[Byte]]] =
      Inflate
        .session(InflateParams(Framing.raw(15), Budget.unlimited))
        .widen[CompressError]
        .use(session => session.update(Slice.of(block.toArray)))
        .absolve
        .map(_.toVector)
        .attempt

    val withReset = emit(reset = true).flatMap(decodeAlone).map {
      case Right(decoded) => assertEquals(decoded, message.toVector)
      case Left(error) => fail(s"post-reset block must decode independently, got $error")
    }
    val withoutReset = emit(reset = false).flatMap(decodeAlone).map {
      case Left(_: CompressError.Malformed) => ()
      case Right(decoded) => assert(decoded != message.toVector, "context-takeover block must not reproduce the message alone")
      case Left(other) => fail(s"unexpected error: $other")
    }
    withReset *> withoutReset
  }
end ZlibSpec
