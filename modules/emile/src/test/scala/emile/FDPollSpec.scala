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

import cats.effect.IO
import cats.effect.Resource

/** Covers [[FDPoll]]: await reports a descriptor that has become readable. */
final class FDPollSpec extends EmileSuite:

  test("await fires Readable when the pipe becomes readable") {
    pipeResource.use: (readFd, writeFd) =>
      val poll = FDPoll.resource(readFd, Set(FDEvent.Readable)).use(_.await).absolve
      val write = IO.sleep(100.millis) *> IO(writeByte(writeFd))
      poll.both(write).timeout(5.seconds).map((events, _) => assert(events.contains(FDEvent.Readable)))
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
