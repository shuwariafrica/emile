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

import java.nio.file.Files
import java.nio.file.Path

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource

// The path-scoped FS operations: metadata, mutation, the streaming directory list, and the
// temporary-directory and temporary-file factories.
final class FSSpec extends EmileSuite:

  test("stat follows a symlink to its target, lstat reports the link itself") {
    tempDir.use: dir =>
      val target = dir.resolve("target.txt")
      val link = dir.resolve("link.txt")
      IO.blocking(Files.write(target, "target".getBytes("UTF-8")): Unit) *>
        FS.symlink(Path.of("target.txt"), link).absolve *>
        (for
          viaStat <- FS.stat(link).absolve
          viaLstat <- FS.lstat(link).absolve
        yield
          assert(viaStat.isFile, "stat did not follow the link to the regular-file target")
          assert(viaLstat.isSymlink, "lstat did not report the symlink itself"))
  }

  test("statfs reports the capacity of the holding filesystem") {
    tempDir.use: dir =>
      FS.statfs(dir)
        .absolve
        .map: info =>
          assert(info.blockSize > 0L, "block size not reported")
          assert(info.blocks > 0L, "total blocks not reported")
  }

  test("access and exists answer both ways without raising") {
    tempDir.use: dir =>
      val present = dir.resolve("present.txt")
      val missing = dir.resolve("missing.txt")
      IO.blocking(Files.write(present, "x".getBytes("UTF-8")): Unit) *>
        FS.chmod(present, 0x100).absolve *> // 0400: readable, not writable
        (for
          existsPresent <- FS.exists(present).absolve
          existsMissing <- FS.exists(missing).absolve
          readable <- FS.access(present, FileAccess.Read).absolve
          writable <- FS.access(present, FileAccess.Write).absolve
        yield
          assert(existsPresent, "present file reported absent")
          assert(!existsMissing, "missing file reported present")
          assert(readable, "readable file reported inaccessible")
          assert(!writable, "read-only file reported writable"))
  }

  test("chmod sets a path's permission bits") {
    tempDir.use: dir =>
      val path = dir.resolve("chmod.txt")
      IO.blocking(Files.write(path, "x".getBytes("UTF-8")): Unit) *>
        FS.chmod(path, 0x1a4).absolve *> // 0644
        FS.stat(path).absolve.map(status => assertEquals(status.permissions, 0x1a4))
  }

  test("setTimes sets explicit access and modification times") {
    tempDir.use: dir =>
      val path = dir.resolve("times.txt")
      IO.blocking(Files.write(path, "t".getBytes("UTF-8")): Unit) *>
        FS.setTimes(path, SetTime.At(FileTime(1_500_000L, 0L)), SetTime.At(FileTime(2_500_000L, 0L))).absolve *>
        FS.stat(path)
          .absolve
          .map: status =>
            assertEquals(status.lastAccess.seconds, 1_500_000L)
            assertEquals(status.lastModified.seconds, 2_500_000L)
  }

  test("rename moves a file and unlink removes it") {
    tempDir.use: dir =>
      val from = dir.resolve("from.txt")
      val to = dir.resolve("to.txt")
      IO.blocking(Files.write(from, "payload".getBytes("UTF-8")): Unit) *>
        FS.rename(from, to).absolve *>
        (for
          fromGone <- FS.exists(from).absolve
          toPresent <- FS.exists(to).absolve
          _ <- FS.unlink(to).absolve
          toGone <- FS.exists(to).absolve
        yield
          assert(!fromGone, "rename left the source in place")
          assert(toPresent, "rename did not create the destination")
          assert(!toGone, "unlink did not remove the file"))
  }

  test("copy overwrites by default and refuses an existing destination without overwrite") {
    tempDir.use: dir =>
      val src = dir.resolve("src.bin")
      val dst = dir.resolve("dst.bin")
      IO.blocking(Files.write(src, "source".getBytes("UTF-8")): Unit) *>
        IO.blocking(Files.write(dst, "old".getBytes("UTF-8")): Unit) *>
        FS.copy(src, dst).absolve *>
        IO.blocking(Files.readAllBytes(dst)).flatMap(read => IO(assertEquals(new String(read, "UTF-8"), "source"))) *>
        FS.copy(src, dst, CopyOptions(overwrite = false, ReflinkMode.None)).either.map {
          case Left(EmileError.IO.AlreadyExists) => ()
          case other => fail(s"expected AlreadyExists on a no-overwrite copy, got: $other")
        }
  }

  test("link creates a hard link to the same content, readlink reads a symlink's target") {
    tempDir.use: dir =>
      val src = dir.resolve("original.txt")
      val hard = dir.resolve("hard.txt")
      val soft = dir.resolve("soft.txt")
      IO.blocking(Files.write(src, "shared".getBytes("UTF-8")): Unit) *>
        FS.link(src, hard).absolve *>
        FS.symlink(Path.of("original.txt"), soft).absolve *>
        (for
          hardContent <- IO.blocking(Files.readAllBytes(hard))
          target <- FS.readlink(soft).absolve
        yield
          assertEquals(new String(hardContent, "UTF-8"), "shared")
          assertEquals(target, Path.of("original.txt")))
  }

  test("realpath resolves to the canonical absolute path") {
    tempDir.use: dir =>
      val path = dir.resolve("real.txt")
      IO.blocking(Files.write(path, "x".getBytes("UTF-8")): Unit) *>
        FS.realpath(dir.resolve(".").resolve("real.txt"))
          .absolve
          .map: resolved =>
            assert(resolved.isAbsolute, "realpath did not return an absolute path")
            assertEquals(resolved.getFileName.toString, "real.txt")
  }

  test("mkdir creates a directory and rmdir refuses a non-empty one") {
    tempDir.use: dir =>
      val made = dir.resolve("made")
      val child = made.resolve("child.txt")
      FS.mkdir(made).absolve *>
        FS.stat(made).absolve.map(status => assert(status.isDirectory, "mkdir did not create a directory")) *>
        IO.blocking(Files.write(child, "x".getBytes("UTF-8")): Unit) *>
        FS.rmdir(made).either.map {
          case Left(EmileError.IO.DirectoryNotEmpty) => ()
          case other => fail(s"expected DirectoryNotEmpty, got: $other")
        } *>
        FS.unlink(child).absolve *>
        FS.rmdir(made).absolve *>
        FS.exists(made).absolve.map(present => assert(!present, "rmdir did not remove the emptied directory"))
  }

  test("list streams a populated directory's entries with their kinds") {
    tempDir.use: dir =>
      val fileA = dir.resolve("a.txt")
      val subdir = dir.resolve("sub")
      IO.blocking(Files.write(fileA, "a".getBytes("UTF-8")): Unit) *>
        IO.blocking(Files.createDirectory(subdir): Unit) *>
        FS.list(dir)
          .compile
          .toList
          .absolve
          .map: entries =>
            val byName = entries.map(e => e.name -> e.kind).toMap
            assertEquals(byName.keySet, Set("a.txt", "sub"))
            // A filesystem without d_type reports Unknown; both real kinds are otherwise expected.
            assert(byName("a.txt") == DirEntryKind.File || byName("a.txt") == DirEntryKind.Unknown)
            assert(byName("sub") == DirEntryKind.Directory || byName("sub") == DirEntryKind.Unknown)
  }

  test("list of an empty directory is an empty stream") {
    tempDir.use: dir =>
      val empty = dir.resolve("empty")
      IO.blocking(Files.createDirectory(empty): Unit) *>
        FS.list(empty).compile.toList.absolve.map(entries => assertEquals(entries, Nil))
  }

  test("tempDirectory creates a directory the caller owns") {
    tempDir.use: dir =>
      FS.tempDirectory(dir.resolve("scratch-XXXXXX").toString)
        .absolve
        .flatMap: created =>
          IO.blocking(Files.isDirectory(created)).map(isDir => assert(isDir, "tempDirectory did not create a directory"))
  }

  test("tempFile yields a writable file that persists after the descriptor closes") {
    val payload = "temp-file-contents".getBytes("UTF-8")
    tempDir.use: dir =>
      FS.tempFile(dir.resolve("scratch-XXXXXX").toString)
        .use(temp => temp.file.write(boilerplate.Slice.of(payload)).as(temp.path))
        .absolve
        .flatMap: path =>
          IO.blocking((Files.exists(path), Files.readAllBytes(path)))
            .map: (exists, read) =>
              assert(exists, "temp file did not persist past the resource scope")
              assertEquals(read.toList, payload.toList)
  }

  private def tempDir: Resource[IO, Path] =
    Resource.make(IO(Files.createTempDirectory("emile-fsspec")))(dir => IO.blocking(deleteRecursively(dir)))

  private def deleteRecursively(path: Path): Unit =
    if Files.isDirectory(path) && !Files.isSymbolicLink(path) then
      val entries = Files.list(path)
      try entries.forEach(deleteRecursively)
      finally entries.close()
    Files.deleteIfExists(path): Unit

end FSSpec
