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

import java.io.File
import java.io.FileOutputStream
import java.nio.file.Path
import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource

// Covers [[FS]]: a real change to a watched path surfaces on `events` and as a coalesced `changes`
// pulse, [[FS$ FS]].poll detects a change by stat-polling, a watch of a missing path fails with a
// typed error, and a second concurrent consumer fails with [[EmileError.IO.ConflictingOperation]].
final class FSWatchSpec extends EmileSuite:

  test("watch reports a change to the watched file") {
    tempFile("emile FS watch probe".getBytes("UTF-8")).use: file =>
      FS.watch(file.toPath)
        .use(fs =>
          EffIO.liftF(
            for
              _ <- IO.blocking(append(file, " changed".getBytes("UTF-8")))
              events <- fs.events.take(1).compile.toList.absolve.timeout(10.seconds)
              _ <- IO(assert(events.exists(_.changes.nonEmpty), s"expected a filesystem change, got: $events"))
            yield ()
          )
        )
        .absolve
  }

  test("watch of a missing path fails with a typed IO error") {
    val missing = Path.of(s"/tmp/emile-fswatch-missing-${System.nanoTime}")
    FS.watch(missing).use(_ => EffIO.liftF(IO.unit)).either.map {
      case Left(_: EmileError.IO) => ()
      case other => fail(s"expected EmileError.IO, got: $other")
    }
  }

  test("changes reports a coalesced pulse on a file change") {
    tempFile("emile FS changes probe".getBytes("UTF-8")).use: file =>
      FS.watch(file.toPath)
        .use(fs =>
          EffIO.liftF(
            for
              _ <- IO.blocking(append(file, " changed".getBytes("UTF-8")))
              pulses <- fs.changes.take(1).compile.toList.absolve.timeout(10.seconds)
              _ <- IO(assertEquals(pulses, List(())))
            yield ()
          )
        )
        .absolve
  }

  test("poll detects a change to the watched file by stat-polling") {
    tempFile("emile FS poll probe".getBytes("UTF-8")).use: file =>
      FS.poll(file.toPath, 200.millis)
        .use(fs =>
          EffIO.liftF(
            for
              // let the first poll record the baseline stat, then change the file's size
              _ <- IO.sleep(500.millis)
              _ <- IO.blocking(append(file, " changed".getBytes("UTF-8")))
              events <- fs.events.take(1).compile.toList.absolve.timeout(15.seconds)
              _ <- IO(assert(events.exists(_.changes.contains(FSChange.Changed)), s"expected a Changed event, got: $events"))
            yield ()
          )
        )
        .absolve
  }

  test("a second concurrent events consumer fails fast with ConflictingOperation") {
    tempFile("emile FS guard probe".getBytes("UTF-8")).use: file =>
      FS.watch(file.toPath)
        .use(fs =>
          EffIO.liftF(
            for
              // the first consumer holds the single-consumer slot; with no change it blocks on take
              consuming <- fs.events.compile.drain.absolve.start
              _ <- IO.sleep(200.millis)
              result <- fs.events.take(1).compile.toList.either
              _ <- consuming.cancel
              _ <- IO(result match
                     case Left(EmileError.IO.ConflictingOperation) => ()
                     case other => fail(s"expected ConflictingOperation, got: $other"))
            yield ()
          )
        )
        .absolve
        .timeout(10.seconds)
  }

  private def tempFile(content: Array[Byte]): Resource[IO, File] =
    Resource.make(IO(writeTempFile(content)))(file => IO(file.delete(): Unit))

  private def writeTempFile(content: Array[Byte]): File =
    val file = File.createTempFile("emile-fswatch", ".tmp")
    append(file, content)
    file

  private def append(file: File, content: Array[Byte]): Unit =
    val out = new FileOutputStream(file, true)
    try out.write(content)
    finally out.close()

end FSWatchSpec
