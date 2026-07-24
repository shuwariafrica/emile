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

import java.nio.file.Path
import scala.compiletime.testing.typeChecks
import scala.concurrent.duration.*
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource

import emile.unsafe.LibUV

final class ProcessSpec extends EmileSuite:

  test("a piped round-trip echoes stdin back through stdout") {
    Process
      .spawn(ProcessConfig("/bin/cat", Nil))
      .widen[EmileError.Spawn | EmileError.IO]
      .use { p =>
        p.stdin.write(bytes("hello uv_spawn")) *> p.stdin.close *> readAll(p.stdout)
      }
      .map(out => assertEquals(out, "hello uv_spawn"))
      .absolve
      .timeout(10.seconds)
  }

  test("stdin, stdout, and stderr accessors resolve only on a piped stream") {
    assert(typeChecks("def f(p: Process[StdioState.Piped, StdioState.Piped, StdioState.Piped]) = p.stdin"))
    assert(typeChecks("def f(p: Process[StdioState.Inherited, StdioState.Piped, StdioState.Piped]) = p.stdout"))
    assert(!typeChecks("def f(p: Process[StdioState.Inherited, StdioState.Piped, StdioState.Piped]) = p.stdin"))
    assert(!typeChecks("def f(p: Process[StdioState.Piped, StdioState.Piped, StdioState.Ignored]) = p.stderr"))
  }

  test("spawning a missing executable fails with NotFound") {
    Process
      .spawn(ProcessConfig("/no/such/executable-xyz-12345", Nil))
      .use(_ => EffIO.unit)
      .either
      .map {
        case Left(EmileError.Spawn.NotFound) => ()
        case other => fail(s"expected Spawn.NotFound, got: $other")
      }
      .timeout(10.seconds)
  }

  test("spawning a non-executable file fails with PermissionDenied") {
    Process
      .spawn(ProcessConfig("/etc/hostname", Nil))
      .use(_ => EffIO.unit)
      .either
      .map {
        case Left(EmileError.Spawn.PermissionDenied) => ()
        case other => fail(s"expected Spawn.PermissionDenied, got: $other")
      }
      .timeout(10.seconds)
  }

  test("an empty executable path is rejected before libuv with InvalidArgument") {
    Process
      .spawn(ProcessConfig("", Nil))
      .use(_ => EffIO.unit)
      .either
      .map {
        case Left(EmileError.Spawn.InvalidArgument(_)) => ()
        case other => fail(s"expected Spawn.InvalidArgument, got: $other")
      }
      .timeout(10.seconds)
  }

  test("a normal exit reports Exited with its code") {
    Process
      .spawn(silent(ProcessConfig("/bin/sh", List("-c", "exit 7"))))
      .widen[EmileError.Spawn | EmileError.IO]
      .use(p => p.status)
      .map(status => assertEquals(status, ExitStatus.Exited(7)))
      .absolve
      .timeout(10.seconds)
  }

  test("a signalled exit reports Signaled with the terminating signal") {
    Process
      .spawn(silent(ProcessConfig("/bin/sleep", List("30"))))
      .widen[EmileError.Spawn | EmileError.IO]
      .use(p => p.kill(SignalNumber.SIGTERM) *> p.status)
      .map(status => assertEquals(status, ExitStatus.Signaled(SignalNumber.SIGTERM)))
      .absolve
      .timeout(10.seconds)
  }

  test("killing an already-reaped child yields AlreadyClosed") {
    Process
      .spawn(silent(ProcessConfig("/bin/sh", List("-c", "exit 0"))))
      .widen[EmileError.Spawn | EmileError.IO]
      .use { p =>
        p.status *> EffIO.liftF(p.kill(SignalNumber.SIGTERM).either).map {
          case Left(EmileError.IO.AlreadyClosed) => ()
          case other => fail(s"expected IO.AlreadyClosed, got: $other")
        }
      }
      .absolve
      .timeout(10.seconds)
  }

  test("an inherited close-on-exec descriptor is handed to the child as fd 3") {
    pipeResource.use { (readFd, writeFd) =>
      Process
        .spawn(
          ProcessConfig("/bin/sh", List("-c", "cat <&3")).ignoreStdin.ignoreStderr.extraFds(List(ExtraFd.InheritFd(readFd)))
        )
        .widen[EmileError.Spawn | EmileError.IO]
        .use { p =>
          EffIO.liftF(IO(writeString(writeFd, "FD3_PAYLOAD_ABC"))) *>
            EffIO.liftF(IO(closeFd(writeFd))) *>
            readAll(p.stdout)
        }
        .map(out => assertEquals(out, "FD3_PAYLOAD_ABC"))
        .absolve
        .timeout(10.seconds)
    }
  }

  test("a piped extra descriptor round-trips through extraInput") {
    Process
      .spawn(ProcessConfig("/bin/sh", List("-c", "cat <&3")).ignoreStdin.ignoreStderr.extraFds(List(ExtraFd.Piped(PipeDirection.Read))))
      .widen[EmileError.Spawn | EmileError.IO]
      .use { p =>
        val input = p.extraInput(3).getOrElse(fail("extra descriptor 3 has no parent write end"))
        input.write(bytes("PIPED_FD3")) *> input.close *> readAll(p.stdout)
      }
      .map(out => assertEquals(out, "PIPED_FD3"))
      .absolve
      .timeout(10.seconds)
  }

  test("Env.Extend merges the parent environment with the overrides") {
    Process
      .spawn(
        silentIn(ProcessConfig("/bin/sh", List("-c", "printf %s \"$EMILE_TEST_VAR\"; [ -n \"$PATH\" ] && printf :pathset")))
          .env(Env.Extend(Map("EMILE_TEST_VAR" -> "merged")))
      )
      .widen[EmileError.Spawn | EmileError.IO]
      .use(p => readAll(p.stdout))
      .map(out => assertEquals(out, "merged:pathset"))
      .absolve
      .timeout(10.seconds)
  }

  test("cwd runs the child in the given working directory") {
    Process
      .spawn(silentIn(ProcessConfig("/bin/sh", List("-c", "pwd"))).cwd(Path.of("/tmp")))
      .widen[EmileError.Spawn | EmileError.IO]
      .use(p => readAll(p.stdout).map(_.trim))
      .map(out => assertEquals(out, "/tmp"))
      .absolve
      .timeout(10.seconds)
  }

  test("TermThenKill escalates to SIGKILL against a child that ignores SIGTERM") {
    Process
      .spawn(silent(ProcessConfig("/bin/sh", List("-c", "trap '' TERM; sleep 30")).onExit(OnExit.TermThenKill(300.millis))))
      .widen[EmileError.Spawn | EmileError.IO]
      .use(p => EffIO.liftF(IO { assert(LibUV.uv_kill(p.pid, 0) == 0, "child is alive during use"); p.pid }))
      .flatMap(pid => EffIO.liftF(IO(assert(LibUV.uv_kill(pid, 0) < 0, "child was reaped after release"))))
      .absolve
      .timeout(15.seconds)
  }

  test("a detached child outlives the loop reference and is killable by pid") {
    Process
      .spawnDetached(ProcessConfig("/bin/sleep", List("30")).ignoreStdin.ignoreStdout.ignoreStderr)
      .flatMap { detached =>
        for
          _ <- EffIO.liftF(IO(assert(LibUV.uv_kill(detached.pid, 0) == 0, "detached child is alive")))
          _ <- detached.kill(SignalNumber.SIGTERM)
          _ <- EffIO.liftF(IO.sleep(300.millis))
        yield assert(LibUV.uv_kill(detached.pid, 0) < 0, "detached child died on the by-pid kill")
      }
      .absolve
      .timeout(15.seconds)
  }

  test("spawnDetached rejects a piped standard stream with InvalidArgument") {
    Process
      .spawnDetached(ProcessConfig("/bin/sleep", List("30")))
      .either
      .map {
        case Left(EmileError.Spawn.InvalidArgument(_)) => ()
        case other => fail(s"expected Spawn.InvalidArgument, got: $other")
      }
      .timeout(10.seconds)
  }

  test("a user SIGCHLD watch coexists with exit reaping") {
    val userWatch: EmIO[EmileError.Spawn | EmileError.IO, Unit] =
      Signal.watch(SignalNumber.SIGCHLD).take(1).compile.drain
    val childRun: EmIO[EmileError.Spawn | EmileError.IO, ExitStatus] =
      Process
        .spawn(silent(ProcessConfig("/bin/sh", List("-c", "sleep 1; exit 0"))))
        .widen[EmileError.Spawn | EmileError.IO]
        .use(p => p.status)
    childRun
      .both(userWatch)
      .map((status, _) => assertEquals(status, ExitStatus.Exited(0)))
      .absolve
      .timeout(15.seconds)
  }

  // Reads a piped output fully as a UTF-8 string, ending at the child's stream close.
  private def readAll(output: ProcessOutput): EmIO[EmileError.IO, String] =
    output.reads.compile.toList.map(chunk => new String(chunk.toArray, "UTF-8"))

  private def bytes(s: String): Slice =
    val array = s.getBytes("UTF-8")
    Slice.of(array, 0, array.length)

  // A child whose three standard streams are all ignored - for exit-status and signalling rows that
  // never touch stdio.
  private def silent[I <: StdioState, O <: StdioState, E <: StdioState](
    config: ProcessConfig[I, O, E]
  ): ProcessConfig[StdioState.Ignored, StdioState.Ignored, StdioState.Ignored] =
    config.ignoreStdin.ignoreStdout.ignoreStderr

  // As silent, but leaving stdout piped (the entry point pipes it) - for rows that read the child.
  private def silentIn[I <: StdioState, O <: StdioState, E <: StdioState](
    config: ProcessConfig[I, O, E]
  ): ProcessConfig[StdioState.Ignored, O, StdioState.Ignored] =
    config.ignoreStdin.ignoreStderr

  // A close-on-exec pipe for the InheritFd row, both ends closed on release (a double close is EBADF,
  // which the helper swallows).
  private def pipeResource: Resource[IO, (Int, Int)] =
    Resource.make(IO(openCloexecPipe()))((readFd, writeFd) => IO { closeFd(readFd); closeFd(writeFd) })

  // FFI: raw pipe, write, and close over native descriptors.
  // scalafix:off DisableSyntax

  // O_CLOEXEC on Linux - so only the descriptor uv_spawn explicitly wires reaches the child.
  private inline val O_CLOEXEC = 0x80000

  private def openCloexecPipe(): (Int, Int) =
    val fds = stackalloc[CInt](2)
    if CPosix.pipe2(fds, O_CLOEXEC) != 0 then throw new RuntimeException("pipe2 failed")
    (fds(0), fds(1))

  private def writeString(fd: Int, payload: String): Unit = Zone.acquire { implicit z =>
    val array = payload.getBytes("UTF-8")
    val buffer = z.alloc(array.length.toCSize).asInstanceOf[Ptr[Byte]]
    var i = 0
    while i < array.length do
      buffer(i) = array(i)
      i += 1
    unistd.write(fd, buffer, array.length.toCSize): Unit
  }

  private def closeFd(fd: Int): Unit = unistd.close(fd): Unit

  // scalafix:on DisableSyntax

end ProcessSpec

// pipe2 is not in scala-native's POSIX bindings; declared locally to create the close-on-exec pipe.
@extern
private object CPosix:
  def pipe2(fds: Ptr[CInt], flags: CInt): CInt = extern
