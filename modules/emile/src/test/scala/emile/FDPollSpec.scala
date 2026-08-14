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
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource

// Covers [[FDPoll]]: one-shot `await` reports a readable descriptor, persistent `awaits` re-arms
// across deliveries on one handle, and using a released watcher is a typed error.
final class FDPollSpec extends EmileSuite:

  test("await fires Readable when the pipe becomes readable") {
    pipeResource.use: (readFd, writeFd) =>
      val poll = FDPoll.resource(readFd, Set(FDEvent.Readable)).use(_.await).absolve
      val write = IO.sleep(100.millis) *> IO(writeByte(writeFd))
      poll.both(write).timeout(5.seconds).map((events, _) => assert(events.contains(FDEvent.Readable)))
  }

  test("awaits re-arms and reports readiness repeatedly on one handle") {
    pipeResource.use: (readFd, writeFd) =>
      // The byte is never read, so the pipe stays readable; each re-armed poll fires again, so one
      // write yields three deliveries only if the stream re-arms across them.
      val events = FDPoll.resource(readFd, Set(FDEvent.Readable)).use(_.awaits.take(3).compile.toList).absolve
      val write = IO.sleep(100.millis) *> IO(writeByte(writeFd))
      events
        .both(write)
        .timeout(5.seconds)
        .map((es, _) => assert(es.size == 3 && es.forall(_.contains(FDEvent.Readable)), s"expected 3 Readable, got: $es"))
  }

  test("await on a released watcher is a typed AlreadyClosed, not a use-after-free") {
    pipeResource.use: (readFd, _) =>
      (for
        leaked <- FDPoll.resource(readFd, Set(FDEvent.Readable)).use(poll => EffIO.succeed(poll)).absolve
        result <- leaked.await.either
      yield assertEquals(result, Left(EmileError.IO.AlreadyClosed): Either[EmileError.IO, Set[FDEvent]]))
        .timeout(5.seconds)
  }

  test("a second concurrent await fails fast with ConflictingOperation") {
    pipeResource.use: (readFd, _) =>
      FDPoll
        .resource(readFd, Set(FDEvent.Readable))
        .use(poll =>
          EffIO.liftF(
            for
              // The pipe is never written, so the first await arms and blocks; the second conflicts.
              blocked <- poll.await.absolve.start
              _ <- IO.sleep(100.millis)
              result <- poll.await.either
              _ <- blocked.cancel
              _ <- IO(result match
                     case Left(EmileError.IO.ConflictingOperation) => ()
                     case other => fail(s"expected ConflictingOperation, got: $other"))
            yield ()
          )
        )
        .absolve
        .timeout(5.seconds)
  }

  private def writeByte(fd: Int): Unit =
    val buf = stackalloc[Byte]()
    !buf = 1.toByte
    unistd.write(fd, buf, 1.toCSize): Unit

  private def pipeResource: Resource[IO, (Int, Int)] =
    Resource.make(IO(openPipe()))((readFd, writeFd) => IO(closePipe(readFd, writeFd)))

  private def openPipe(): (Int, Int) =
    val fds = stackalloc[CInt](2)
    unistd.pipe(fds): Unit
    (fds(0), fds(1))

  private def closePipe(readFd: Int, writeFd: Int): Unit =
    unistd.close(readFd): Unit
    unistd.close(writeFd): Unit

end FDPollSpec
