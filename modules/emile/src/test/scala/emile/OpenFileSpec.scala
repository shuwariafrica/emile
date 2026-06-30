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

  private def tempFile(content: Array[Byte]): Resource[IO, Path] =
    Resource.make(IO(writeTempFile(content)))(file => IO(file.delete(): Unit)).map(_.toPath)

  private def writeTempFile(content: Array[Byte]): File =
    val file = File.createTempFile("emile-openfile", ".tmp")
    val out = new FileOutputStream(file)
    try out.write(content)
    finally out.close()
    file

end OpenFileSpec
