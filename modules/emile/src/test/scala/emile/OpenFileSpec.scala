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
import scala.scalanative.posix.sys.stat
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.toCString
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk

/** Covers [[OpenFile]]: size, single reads to end of file, and the whole-file `reads` stream. */
final class OpenFileSpec extends EmileSuite:

  test("size reports the file's byte count") {
    val content = "emile OpenFile size probe".getBytes("UTF-8")
    tempFile(content).use: path =>
      OpenFile.open(path).use(_.size).absolve.assertEquals(content.length.toLong)
  }

  test("open of a missing path fails with a typed IO error") {
    val missingPath = Path.of(s"/tmp/emile-test-missing-${System.nanoTime}")
    OpenFile.open(missingPath).use(_.size).either.map {
      case Left(_: EmileError.IO) => ()
      case other => fail(s"expected EmileError.IO, got: $other")
    }
  }

  test("read returns the file content then None at end of file") {
    val content = "emile OpenFile read probe".getBytes("UTF-8")
    tempFile(content).use: path =>
      OpenFile
        .open(path)
        .use(file =>
          EffIO.liftF(
            for
              first <- file.read(4096).absolve
              second <- file.read(4096).absolve
              _ <- IO(assertEquals(first.fold(List.empty[Byte])(_.toList), content.toList))
              _ <- IO(assert(second.isEmpty))
            yield ()
          )
        )
        .absolve
  }

  test("reads streams the whole file") {
    val content = "emile OpenFile reads streaming probe payload".getBytes("UTF-8")
    tempFile(content).use: path =>
      OpenFile
        .open(path)
        .use(file =>
          EffIO.liftF(
            for
              read <- file.reads.compile.to(Chunk).absolve
              _ <- IO(assertEquals(read.toList, content.toList))
            yield ()
          )
        )
        .absolve
  }

  test("reads reuses its buffer across chunks and streams a multi-chunk file intact") {
    // Larger than one 64 KB read buffer, so reads pulls several chunks through the single reused buffer;
    // a byte pattern means a chunk overwritten before its copy would corrupt the result.
    val content = Array.tabulate(200000)(i => (i % 251).toByte)
    tempFile(content).use: path =>
      OpenFile
        .open(path)
        .use(file => file.reads.compile.to(Chunk))
        .absolve
        .map(read => assert(read.toArray.sameElements(content), "reused-buffer reads corrupted the stream"))
  }

  test("an open cancelled while blocked, then completing, closes the orphaned descriptor rather than leaking it") {
    // A read-only FIFO open blocks until a writer appears, so the timeout cancels it while the worker is
    // still inside open() - uv_cancel cannot stop it. Opening a writer then lets the open complete into
    // openDeliver with the cancelled flag set, which closes the now-orphaned fd. Iterated, with the
    // process descriptor count checked before and after, so a leaked descriptor per iteration would show.
    fifo.use: path =>
      val cancelBlockedOpen = OpenFile.open(Path.of(path)).use(_ => EffIO.succeed(())).absolve.timeout(200.millis).attempt.void
      val unblock = IO.blocking { val w = new FileOutputStream(path); w.close() }
      (for
        before <- fdCount
        _ <- (cancelBlockedOpen *> unblock *> IO.sleep(100.millis)).replicateA_(8)
        after <- fdCount
        _ <- IO(assert(after <= before + 2, s"orphaned descriptors leaked: $before -> $after"))
      yield ()).timeout(30.seconds)
  }

  private def tempFile(content: Array[Byte]): Resource[IO, Path] =
    Resource.make(IO(writeTempFile(content)))(file => IO(file.delete(): Unit)).map(_.toPath)

  private def fifo: Resource[IO, String] =
    Resource.make(IO.blocking(makeFifo()))(path => IO.blocking(new File(path).delete(): Unit))

  private def makeFifo(): String =
    val path = s"/tmp/emile-openfile-fifo-${System.nanoTime}"
    val rc = Zone(stat.mkfifo(toCString(path), 438.toUInt)) // 0666, further masked by umask
    if rc != 0 then throw new RuntimeException(s"mkfifo failed with $rc") // scalafix:ok DisableSyntax.throw
    path

  private def fdCount: IO[Int] = IO.blocking(new File("/proc/self/fd").list().length)

  private def writeTempFile(content: Array[Byte]): File =
    val file = File.createTempFile("emile-openfile", ".tmp")
    val out = new FileOutputStream(file)
    try out.write(content)
    finally out.close()
    file

end OpenFileSpec
