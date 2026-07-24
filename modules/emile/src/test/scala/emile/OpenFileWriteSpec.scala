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
import java.nio.file.Files
import java.nio.file.Path
import scala.concurrent.duration.*
import scala.scalanative.posix.sys.stat
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.toCString
import scala.scalanative.unsigned.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Stream

// OpenFile's write side: open modes, positioned and appending writes, the streaming sink,
// truncate/sync, and the fstat-backed metadata and mutation operations.
final class OpenFileWriteSpec extends EmileSuite:

  test("Write mode creates and truncates, round-tripping the payload") {
    val payload = "emile write-mode payload".getBytes("UTF-8")
    tempDir.use: dir =>
      val path = dir.resolve("created.bin")
      OpenFile.open(path, OpenMode.Write).use(_.write(Slice.of(payload))).absolve *>
        readBack(path).map(read => assertEquals(read.toList, payload.toList))
  }

  test("Append mode appends after existing content") {
    val first = "one\n".getBytes("UTF-8")
    val second = "two\n".getBytes("UTF-8")
    tempDir.use: dir =>
      val path = dir.resolve("appended.log")
      OpenFile.open(path, OpenMode.Write).use(_.write(Slice.of(first))).absolve *>
        OpenFile.open(path, OpenMode.Append).use(_.write(Slice.of(second))).absolve *>
        readBack(path).map(read => assertEquals(new String(read, "UTF-8"), "one\ntwo\n"))
  }

  test("CreateNew fails with AlreadyExists on an existing file") {
    tempDir.use: dir =>
      val path = dir.resolve("exclusive.bin")
      IO.blocking(Files.write(path, "existing".getBytes("UTF-8")): Unit) *>
        OpenFile.open(path, OpenMode.CreateNew).use(_ => EffIO.succeed(())).either.map {
          case Left(EmileError.IO.AlreadyExists) => ()
          case other => fail(s"expected AlreadyExists, got: $other")
        }
  }

  test("a positioned write lands at the offset without moving the file position") {
    tempDir.use: dir =>
      val path = dir.resolve("positioned.bin")
      OpenFile
        .open(path, OpenMode.Write)
        .use(file =>
          file.write(Slice.of("0123456789".getBytes("UTF-8"))) *>
            file.write(Slice.of("AB".getBytes("UTF-8")), 3L)
        )
        .absolve *>
        readBack(path).map(read => assertEquals(new String(read, "UTF-8"), "012AB56789"))
  }

  test("an append-mode write ignores the positioned offset and still appends") {
    tempDir.use: dir =>
      val path = dir.resolve("append-offset.bin")
      OpenFile.open(path, OpenMode.Write).use(_.write(Slice.of("012345".getBytes("UTF-8")))).absolve *>
        OpenFile.open(path, OpenMode.Append).use(_.write(Slice.of("XYZ".getBytes("UTF-8")), 0L)).absolve *>
        readBack(path).map(read => assertEquals(new String(read, "UTF-8"), "012345XYZ"))
  }

  test("loop-to-completion writes a multi-chunk payload intact") {
    // Larger than any single kernel write is likely to accept at once, with a byte pattern so a lost
    // or misplaced resubmission would corrupt the read-back.
    val payload = Array.tabulate(500000)(i => (i % 251).toByte)
    tempDir.use: dir =>
      val path = dir.resolve("large.bin")
      OpenFile.open(path, OpenMode.Write).use(_.write(Slice.of(payload))).absolve *>
        readBack(path).map(read => assert(read.sameElements(payload), "loop-to-completion write corrupted the payload"))
  }

  test("truncate shrinks the file and sync flushes it") {
    tempDir.use: dir =>
      val path = dir.resolve("truncated.bin")
      OpenFile
        .open(path, OpenMode.Write)
        .use(file => file.write(Slice.of("0123456789".getBytes("UTF-8"))) *> file.truncate(4L) *> file.sync)
        .absolve *>
        readBack(path).map(read => assertEquals(new String(read, "UTF-8"), "0123"))
  }

  test("the writes pipe appends every chunk of a stream") {
    val payload = Array.tabulate(1000)(i => (i % 256).toByte)
    tempDir.use: dir =>
      val path = dir.resolve("piped.bin")
      OpenFile
        .open(path, OpenMode.Write)
        .use(file => Stream.emits(payload).covary[EmIO.Of[EmileError.IO]].through(file.writes).compile.drain)
        .absolve *>
        readBack(path).map(read => assert(read.sameElements(payload), "the writes pipe dropped or reordered bytes"))
  }

  test("stat reports size, a regular-file type, and a modification time") {
    val payload = "emile stat-field probe".getBytes("UTF-8")
    tempDir.use: dir =>
      val path = dir.resolve("stat.bin")
      OpenFile
        .open(path, OpenMode.Write)
        .use(file => file.write(Slice.of(payload)) *> file.stat)
        .absolve
        .map: status =>
          assertEquals(status.size, payload.length.toLong)
          assert(status.isFile, "regular file not reported as such")
          assert(!status.isDirectory)
          assert(status.lastModified.seconds > 0L, "modification time not populated")
  }

  test("chmod sets the permission bits, read back through stat") {
    tempDir.use: dir =>
      val path = dir.resolve("chmod.bin")
      OpenFile
        .open(path, OpenMode.Write)
        .use(file => file.write(Slice.of("x".getBytes("UTF-8"))) *> file.chmod(0x180) *> file.stat)
        .absolve
        .map(status => assertEquals(status.permissions, 0x180)) // 0600
  }

  test("setTimes sets explicit times, leaves an omitted one, and applies now") {
    tempDir.use: dir =>
      val path = dir.resolve("times.bin")
      OpenFile
        .open(path, OpenMode.Write)
        .use(file =>
          for
            _ <- file.write(Slice.of("t".getBytes("UTF-8")))
            _ <- file.setTimes(SetTime.At(FileTime(1_000_000L, 0L)), SetTime.At(FileTime(2_000_000L, 0L)))
            explicit <- file.stat
            _ <- file.setTimes(SetTime.Now, SetTime.Omit)
            afterNow <- file.stat
          yield
            assertEquals(explicit.lastAccess.seconds, 1_000_000L)
            assertEquals(explicit.lastModified.seconds, 2_000_000L)
            assertEquals(afterNow.lastModified.seconds, 2_000_000L) // Omit left it unchanged
            assert(afterNow.lastAccess.seconds > 1_000_000L, "Now did not advance the access time")
        )
        .absolve
  }

  test("a write-mode open cancelled while blocked completes cleanly rather than hanging") {
    // A write-only FIFO open blocks until a reader appears; the timeout cancels it while blocked. A
    // reader then unblocks any open the cancel could not stop, whose fd the orphan path closes.
    fifo.use: path =>
      val cancelBlockedOpen =
        OpenFile.open(Path.of(path), OpenMode.Write).use(_ => EffIO.succeed(())).absolve.timeout(200.millis).attempt.void
      val unblock = IO.blocking { val r = new java.io.FileInputStream(path); r.close() }
      (cancelBlockedOpen *> unblock *> IO.sleep(100.millis)).timeout(10.seconds)
  }

  private def readBack(path: Path): IO[Array[Byte]] = IO.blocking(Files.readAllBytes(path))

  private def tempDir: Resource[IO, Path] =
    Resource.make(IO(Files.createTempDirectory("emile-openfile-write")))(dir => IO.blocking(deleteRecursively(dir)))

  private def deleteRecursively(path: Path): Unit =
    if Files.isDirectory(path) && !Files.isSymbolicLink(path) then
      val entries = Files.list(path)
      try entries.forEach(deleteRecursively)
      finally entries.close()
    Files.deleteIfExists(path): Unit

  private def fifo: Resource[IO, String] =
    Resource.make(IO.blocking(makeFifo()))(path => IO.blocking(new File(path).delete(): Unit))

  private def makeFifo(): String =
    val path = s"/tmp/emile-openfile-write-fifo-${System.nanoTime}"
    val rc = Zone(stat.mkfifo(toCString(path), 438.toUInt)) // 0666, further masked by umask
    if rc != 0 then throw new RuntimeException(s"mkfifo failed with $rc") // scalafix:ok DisableSyntax.throw
    path

end OpenFileWriteSpec
