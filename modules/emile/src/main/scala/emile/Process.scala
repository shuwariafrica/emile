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
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.signal
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.util.boundary

import boilerplate.Slice
import boilerplate.effect.EffIO
import boilerplate.nullable.*
import cats.effect.IO
import cats.effect.Resource

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing

/** The type-level state of one of a child's standard streams, tracked in [[Process]]'s and
  * [[ProcessConfig]]'s phantom parameters so that a direction-typed stdio accessor resolves only
  * when the stream is actually piped. Erased at runtime; the variants are in
  * [[StdioState$ StdioState]].
  */
sealed trait StdioState

/** The [[StdioState]] phantoms: a piped stream (a parent pipe end), an inherited one (the parent's
  * own descriptor), or an ignored one (redirected to `/dev/null`).
  */
object StdioState:

  /** A piped stream: emile holds the parent end, exposed as a write or read surface on [[Process]]. */
  sealed trait Piped extends StdioState

  /** An inherited stream: the child shares the parent's corresponding descriptor. */
  sealed trait Inherited extends StdioState

  /** An ignored stream: the child's descriptor is redirected to `/dev/null`. */
  sealed trait Ignored extends StdioState

// The runtime mirror of a StdioState phantom, carried as a value in ProcessConfig so the spawn
// marshalling can build the right uv_stdio_container_t for a phantom the type system erases.
private[emile] enum Stdio derives CanEqual:
  case Piped, Inherited, Ignored

/** The direction of an extra piped descriptor, from the child's perspective: [[Read]] means the
  * child reads and the parent writes, [[Write]] means the child writes and the parent reads,
  * [[Duplex]] both. Used in [[ExtraFd.Piped]].
  */
enum PipeDirection derives CanEqual:
  case Read, Write, Duplex

/** An extra child descriptor beyond the three standard streams, given in [[ProcessConfig.extraFds]]
  * in ascending order so the nth entry (0-based) becomes the child's descriptor `3 + n`.
  */
enum ExtraFd derives CanEqual:

  /** A pipe emile creates and holds the parent end of, reachable through [[Process.extraInput]] /
    * [[Process.extraOutput]]. `direction` is the child's perspective.
    */
  case Piped(direction: PipeDirection)

  /** An already-open caller descriptor duplicated into the child's slot. The caller owns it: it
    * MUST be created close-on-exec so only this slot reaches the child, and the caller closes its
    * own ends.
    */
  case InheritFd(fd: Int)

/** The child's environment. libuv has no merge, so [[Extend]] merges the parent environment
  * emile-side at spawn time; [[Replace]] hands the child exactly the given variables and nothing
  * else.
  */
enum Env derives CanEqual:

  /** The child inherits the parent's environment unchanged - the default. */
  case Inherit

  /** The child's environment is exactly `vars`; nothing is inherited. */
  case Replace(vars: Map[String, String])

  /** The parent's environment with `overrides` applied on top, an override winning over an
    * inherited variable of the same name.
    */
  case Extend(overrides: Map[String, String])

/** The policy applied to a still-running child when a [[Process]] resource releases. The parent
  * always awaits the child's reaping before the handle is closed, so a reaped child never lingers
  * as a zombie - except where [[Term]]'s grace elapses without the child exiting.
  */
enum OnExit derives CanEqual:

  /** Wait for the child to exit on its own - no signal is sent, so release blocks until it does. */
  case Await

  /** Send `SIGKILL` and await the reaping. */
  case Kill

  /** Send `SIGTERM` and await the reaping for up to `grace`; if the child outlives the grace it
    * keeps running and, once the handle closes, is reaped only when the parent process exits.
    */
  case Term(grace: FiniteDuration)

  /** Send `SIGTERM`, await the reaping for up to `grace`, then `SIGKILL` and await it if the child
    * is still alive - the terminate-then-force ladder, and the [[ProcessConfig]] default.
    */
  case TermThenKill(grace: FiniteDuration)
end OnExit

/** How a child ended: [[Exited]] with its 0-255 status code, or [[Signaled]] by the signal that
  * terminated it. The two are mutually exclusive, mirroring the single `exit_cb` invocation.
  */
enum ExitStatus derives CanEqual:
  case Exited(code: Int)
  case Signaled(signal: SignalNumber)

/** A detached child - spawned into its own session and unreferenced from the loop, so it outlives
  * the runtime. It exposes only its [[Detached.pid]] and [[Detached.kill]]; there is no stdio and
  * no supervision scope. Operations are on [[Detached$ Detached]].
  */
opaque type Detached = Int

/** The identifier and signalling of a [[Detached]] child. */
object Detached:

  given CanEqual[Detached, Detached] = CanEqual.derived

  private[emile] inline def wrap(pid: Int): Detached = pid

  extension (detached: Detached)
    /** The child's process id. */
    def pid: Int = detached

    /** Send `signal` to the child by pid. `ESRCH` (the child has exited and been reaped) maps to
      * [[EmileError.IO.AlreadyClosed]]. Since it signals by pid, a pid the kernel has recycled
      * could in principle be signalled instead - inherent to detached, best-effort signalling.
      */
    def kill(signal: SignalNumber): EmIO[EmileError.IO, Unit] = Process.killDetached(detached, signal)

/** A description of a child process to spawn: the executable, its arguments, environment, working
  * directory, credentials, stdio wiring, and the release policy. Immutable; the phantom parameters
  * `I`, `O`, `E` track the stdin, stdout, and stderr [[StdioState]]. Build one through
  * [[ProcessConfig$ ProcessConfig]], whose entry point pipes all three streams and whose preset
  * transitions inherit or ignore them.
  */
final class ProcessConfig[I <: StdioState, O <: StdioState, E <: StdioState] private[emile] (
  val file: String,
  val args: List[String],
  private[emile] val stdinState: Stdio,
  private[emile] val stdoutState: Stdio,
  private[emile] val stderrState: Stdio,
  private[emile] val envSetting: Env,
  private[emile] val cwdPath: Option[Path],
  private[emile] val creds: Option[(Int, Int)],
  private[emile] val exitPolicy: OnExit,
  private[emile] val extraFdList: List[ExtraFd]
)

/** Construction and the builder transitions for [[ProcessConfig]]. The entry point pipes all three
  * standard streams (the `argv(0)` passed to the child is always `file`); presets change a stream's
  * [[StdioState]] at the type level, and the remaining setters fill in environment, working
  * directory, credentials, extra descriptors, and the [[OnExit]] policy.
  */
object ProcessConfig:

  // The default terminate-then-force grace. A middle ground: long enough for an orderly child to
  // clean up on SIGTERM, short enough that a stuck child does not stall a resource release for long.
  private val DefaultGrace: FiniteDuration = 5.seconds

  /** A config for `file` with `args` (the arguments after `argv(0)`), all three streams piped. */
  def apply(file: String, args: List[String]): ProcessConfig[StdioState.Piped, StdioState.Piped, StdioState.Piped] =
    new ProcessConfig(file, args, Stdio.Piped, Stdio.Piped, Stdio.Piped, Env.Inherit, None, None, OnExit.TermThenKill(DefaultGrace), Nil)

  /** A config for `file` with the arguments given as varargs rather than a list; otherwise as the
    * list-taking constructor.
    */
  def apply(file: String, args: String*): ProcessConfig[StdioState.Piped, StdioState.Piped, StdioState.Piped] =
    apply(file, args.toList)

  extension [I <: StdioState, O <: StdioState, E <: StdioState](c: ProcessConfig[I, O, E])

    /** Inherit the parent's standard input rather than piping it. */
    def inheritStdin: ProcessConfig[StdioState.Inherited, O, E] =
      new ProcessConfig(
        c.file,
        c.args,
        Stdio.Inherited,
        c.stdoutState,
        c.stderrState,
        c.envSetting,
        c.cwdPath,
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Redirect the child's standard input to `/dev/null`. */
    def ignoreStdin: ProcessConfig[StdioState.Ignored, O, E] =
      new ProcessConfig(
        c.file,
        c.args,
        Stdio.Ignored,
        c.stdoutState,
        c.stderrState,
        c.envSetting,
        c.cwdPath,
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Inherit the parent's standard output rather than piping it. */
    def inheritStdout: ProcessConfig[I, StdioState.Inherited, E] =
      new ProcessConfig(
        c.file,
        c.args,
        c.stdinState,
        Stdio.Inherited,
        c.stderrState,
        c.envSetting,
        c.cwdPath,
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Redirect the child's standard output to `/dev/null`. */
    def ignoreStdout: ProcessConfig[I, StdioState.Ignored, E] =
      new ProcessConfig(
        c.file,
        c.args,
        c.stdinState,
        Stdio.Ignored,
        c.stderrState,
        c.envSetting,
        c.cwdPath,
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Inherit the parent's standard error rather than piping it. */
    def inheritStderr: ProcessConfig[I, O, StdioState.Inherited] =
      new ProcessConfig(
        c.file,
        c.args,
        c.stdinState,
        c.stdoutState,
        Stdio.Inherited,
        c.envSetting,
        c.cwdPath,
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Redirect the child's standard error to `/dev/null`. */
    def ignoreStderr: ProcessConfig[I, O, StdioState.Ignored] =
      new ProcessConfig(
        c.file,
        c.args,
        c.stdinState,
        c.stdoutState,
        Stdio.Ignored,
        c.envSetting,
        c.cwdPath,
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Set the child's [[Env environment]]. */
    def env(env: Env): ProcessConfig[I, O, E] =
      new ProcessConfig(c.file, c.args, c.stdinState, c.stdoutState, c.stderrState, env, c.cwdPath, c.creds, c.exitPolicy, c.extraFdList)

    /** Run the child with `path` as its working directory rather than inheriting the parent's. */
    def cwd(path: Path): ProcessConfig[I, O, E] =
      new ProcessConfig(
        c.file,
        c.args,
        c.stdinState,
        c.stdoutState,
        c.stderrState,
        c.envSetting,
        Some(path),
        c.creds,
        c.exitPolicy,
        c.extraFdList
      )

    /** Run the child as `uid` / `gid` (via `setuid` / `setgid`); requires the parent to have the
      * privilege to switch. Values are the platform's numeric ids.
      */
    def credentials(uid: Int, gid: Int): ProcessConfig[I, O, E] =
      new ProcessConfig(
        c.file,
        c.args,
        c.stdinState,
        c.stdoutState,
        c.stderrState,
        c.envSetting,
        c.cwdPath,
        Some((uid, gid)),
        c.exitPolicy,
        c.extraFdList
      )

    /** Set the resource-release [[OnExit]] policy. */
    def onExit(policy: OnExit): ProcessConfig[I, O, E] =
      new ProcessConfig(c.file, c.args, c.stdinState, c.stdoutState, c.stderrState, c.envSetting, c.cwdPath, c.creds, policy, c.extraFdList)

    /** Set the [[ExtraFd extra descriptors]] beyond stdio, in ascending child-descriptor order. */
    def extraFds(fds: List[ExtraFd]): ProcessConfig[I, O, E] =
      new ProcessConfig(c.file, c.args, c.stdinState, c.stdoutState, c.stderrState, c.envSetting, c.cwdPath, c.creds, c.exitPolicy, fds)

  end extension

end ProcessConfig

/** The parent's write end of a child input pipe - the child's standard input, or an extra
  * descriptor the child reads. Operations are on [[ProcessInput$ ProcessInput]]; it shares the
  * byte-stream write path of a [[Socket$ Socket]].
  */
opaque type ProcessInput = StreamState

/** The write operations of a [[ProcessInput]]. */
object ProcessInput:

  given CanEqual[ProcessInput, ProcessInput] = CanEqual.derived

  private[emile] inline def wrap(state: StreamState): ProcessInput = state

  extension (input: ProcessInput)

    /** Write `slice` to the child with no copy. The region is borrowed until the effect completes,
      * so do not mutate it while the write is in flight.
      */
    def write(slice: Slice): EmIO[EmileError.IO, Unit] = StreamCore.writeSlice(input, slice)

    /** A pipe writing every byte the source emits to the child, chunk-by-chunk. */
    def writes: EmPipe[EmileError.IO, Byte, Nothing] = StreamCore.writes(input)

    /** Close this input, so the child reads end-of-file. Sequence it after your writes have
      * completed; a subsequent write fails with [[EmileError.IO.AlreadyClosed]]. Idempotent.
      */
    def close: EmIO[EmileError.IO, Unit] = Process.closeInput(input)
end ProcessInput

/** The parent's read end of a child output pipe - the child's standard output or error, or an extra
  * descriptor the child writes. Operations are on [[ProcessOutput$ ProcessOutput]]; it shares the
  * byte-stream read path of a [[Socket$ Socket]]. Each output has a single reader: a second
  * concurrent read fails with [[EmileError.IO.ConflictingOperation]].
  */
opaque type ProcessOutput = StreamState

/** The read operations of a [[ProcessOutput]]. */
object ProcessOutput:

  given CanEqual[ProcessOutput, ProcessOutput] = CanEqual.derived

  private[emile] inline def wrap(state: StreamState): ProcessOutput = state

  extension (output: ProcessOutput)

    /** A back-pressured byte stream over a persistent read, ending when the child closes the
      * stream.
      */
    def reads: EmStream[EmileError.IO, Byte] = StreamCore.reads(output)

    /** Reads one chunk and hands `f` a borrowed [[boilerplate.Slice Slice]] over the receive
      * buffer, sparing a copy. The slice is valid only while `f` runs, so `f` must not retain it.
      * `None` at end of output.
      */
    inline def read[E <: Throwable, A](f: Slice => EmIO[E, A]): EmIO[EmileError.IO | E, Option[A]] =
      StreamCore.readPtrOnce(output, f)

    /** Reads continuously, running `onChunk` inline on the owning loop thread with a borrowed
      * [[boilerplate.Slice Slice]] over each chunk until end of output. `onChunk` must neither
      * block nor retain its slice; a `Left(e)` stops the read early.
      */
    inline def consume[E <: Throwable](onChunk: Slice => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
      StreamCore.consumeAll(output, onChunk)
  end extension
end ProcessOutput

// The exit-status delivery for a managed process, confined to the owning loop thread: exit_cb records
// the status and fires every awaiting waiter, and status / tryStatus read it there. A cats-effect
// Deferred cannot be completed from a raw libuv callback (no dispatcher), so this is the bridge.
final private class ProcessExit:
  var status: ExitStatus | Null = null // scalafix:ok DisableSyntax.var, DisableSyntax.null
  var waiters: List[ExitStatus => Unit] = Nil // scalafix:ok DisableSyntax.var

// Carries the owning poller so the detached self-close callback can release the handle's anchor.
final private class DetachedExit(val poller: LibUVPoller)

// A parent pipe end for an extra descriptor, tagged with its child-perspective direction so the
// extraInput / extraOutput accessors expose only the valid surface.
final private class ExtraState(val stream: StreamState, val direction: PipeDirection)

/** A running child process, backed by a `uv_process_t`. Acquired through
  * [[Process$ Process]].spawn; the phantom parameters `I`, `O`, `E` carry the stdin, stdout, and
  * stderr [[StdioState]] so that [[Process.stdin]], [[Process.stdout]], and [[Process.stderr]]
  * resolve only on a piped stream - reading a stream that was not piped is a compile error.
  * [[Process.status]] awaits the exit, [[Process.kill]] signals the child, and the resource release
  * applies the config's [[OnExit]] policy then reaps and closes the handle.
  */
opaque type Process[I <: StdioState, O <: StdioState, E <: StdioState] = ProcessState

// The mutable and immutable per-process state: the live handle, pid, release policy, the exit bridge,
// the parent pipe ends of the piped streams (null when not piped), the extra pipes by child descriptor,
// and every parent pipe state to release.
final private class ProcessState(
  val live: LiveHandle,
  val poller: LibUVPoller,
  val pid: Int,
  val exitPolicy: OnExit,
  val exit: ProcessExit,
  val stdinPipe: StreamState | Null,
  val stdoutPipe: StreamState | Null,
  val stderrPipe: StreamState | Null,
  val extras: Map[Int, ExtraState],
  val pipeStates: List[StreamState]
)

/** The subprocess surface: [[spawn]] and [[spawnDetached]], the [[disableStdioInheritance]]
  * control, and the accessors, signalling, and exit-status operations on a [[Process]]. A spawn
  * runs on the current worker's loop; the resulting process, its pipes, and its reaping are all
  * driven by that loop. Spawn failure is synchronous and typed [[EmileError.Spawn]] - the exit
  * callback never fires for a failed exec.
  */
object Process:

  given [I <: StdioState, O <: StdioState, E <: StdioState] => CanEqual[Process[I, O, E], Process[I, O, E]] = CanEqual.derived

  /** Spawn `config`'s child on the current worker's loop. The process is a resource: on release the
    * config's [[OnExit]] policy is applied, the child is reaped, and the handle and every parent
    * pipe end are closed. Fails with [[EmileError.Spawn]] - `NotFound` for an unresolvable
    * executable, `PermissionDenied` for a non-executable one.
    */
  def spawn[I <: StdioState, O <: StdioState, E <: StdioState](
    config: ProcessConfig[I, O, E]): EmResource[EmileError.Spawn, Process[I, O, E]] =
    Resource.make[EffIO.Of[EmileError.Spawn], Process[I, O, E]](acquireManaged(config))(p => EffIO.liftF(release(p)))

  /** Spawn `config`'s child detached: it runs in its own session, its loop reference is dropped so
    * the runtime need not stay alive for it, and only its [[Detached]] handle (pid and kill) is
    * returned. The standard streams must be inherited or ignored - a detached child cannot pipe a
    * stream nothing would own - and any [[ExtraFd]] must be [[ExtraFd.InheritFd]], else this fails
    * with [[EmileError.Spawn.InvalidArgument]].
    */
  def spawnDetached[I <: StdioState, O <: StdioState, E <: StdioState](config: ProcessConfig[I, O, E]): EmIO[EmileError.Spawn, Detached] =
    acquireDetached(config)

  /** Set close-on-exec on every descriptor this process has already inherited, so descriptors
    * opened before this call do not leak into later children. Best-effort and process-global (an
    * unlocked `fcntl` sweep): call it once at startup, before any emile descriptor exists, and
    * prefer creating handed-off descriptors close-on-exec ([[ExtraFd.InheritFd]]) as the
    * load-bearing discipline.
    */
  def disableStdioInheritance: EmIO[Nothing, Unit] = EffIO.suspend(LibUV.uv_disable_stdio_inheritance())

  extension [I <: StdioState, O <: StdioState, E <: StdioState](p: Process[I, O, E])

    /** The child's process id. */
    def pid: Int = (p: ProcessState).pid

    /** Send `signal` to the child. `ESRCH` (the child has exited and been reaped) maps to
      * [[EmileError.IO.AlreadyClosed]].
      */
    def kill(signal: SignalNumber): EmIO[EmileError.IO, Unit] = killEff(p, signal)

    /** The child's [[ExitStatus]], awaiting the exit if it has not yet happened. Independent of the
      * stdio streams - stream end and exit have no guaranteed order, so await each on its own.
      */
    def status: EmIO[Nothing, ExitStatus] = statusEff(p)

    /** The child's [[ExitStatus]] if it has already exited, else `None` - the non-blocking poll of
      * [[status]], with no signalling syscall.
      */
    def tryStatus: EmIO[Nothing, Option[ExitStatus]] = tryStatusEff(p)

    /** The parent write end of the extra descriptor `childFd`, if that descriptor is a piped one
      * the child reads ([[PipeDirection.Read]] or [[PipeDirection.Duplex]]); `None` otherwise. The
      * nth extra descriptor (0-based) is `childFd == 3 + n`.
      */
    def extraInput(childFd: Int): Option[ProcessInput] = extraInputOf(p, childFd)

    /** The parent read end of the extra descriptor `childFd`, if that descriptor is a piped one the
      * child writes ([[PipeDirection.Write]] or [[PipeDirection.Duplex]]); `None` otherwise.
      */
    def extraOutput(childFd: Int): Option[ProcessOutput] = extraOutputOf(p, childFd)

  end extension

  extension [O <: StdioState, E <: StdioState](p: Process[StdioState.Piped, O, E])
    /** The child's standard input, as a [[ProcessInput]] write surface. */
    def stdin: ProcessInput =
      val pipe: StreamState | Null = (p: ProcessState).stdinPipe
      ProcessInput.wrap(pipe.unsafe("stdin is not piped"))

  extension [I <: StdioState, E <: StdioState](p: Process[I, StdioState.Piped, E])
    /** The child's standard output, as a [[ProcessOutput]] read surface. */
    def stdout: ProcessOutput =
      val pipe: StreamState | Null = (p: ProcessState).stdoutPipe
      ProcessOutput.wrap(pipe.unsafe("stdout is not piped"))

  extension [I <: StdioState, O <: StdioState](p: Process[I, O, StdioState.Piped])
    /** The child's standard error, as a [[ProcessOutput]] read surface. */
    def stderr: ProcessOutput =
      val pipe: StreamState | Null = (p: ProcessState).stderrPipe
      ProcessOutput.wrap(pipe.unsafe("stderr is not piped"))

  // Close a child input by reclaiming its parent pipe end - the descriptor closing sends the child
  // end-of-file. Idempotent through LiveHandle; the resource release then finds it already closed.
  private[emile] def closeInput(state: StreamState): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(LiveHandle.closeOnOwner(state.live), EmileError.IO.Unexpected(_))

  private def extraInputOf(state: ProcessState, childFd: Int): Option[ProcessInput] =
    state.extras
      .get(childFd)
      .filter(e => e.direction == PipeDirection.Read || e.direction == PipeDirection.Duplex)
      .map(e => ProcessInput.wrap(e.stream))

  private def extraOutputOf(state: ProcessState, childFd: Int): Option[ProcessOutput] =
    state.extras
      .get(childFd)
      .filter(e => e.direction == PipeDirection.Write || e.direction == PipeDirection.Duplex)
      .map(e => ProcessOutput.wrap(e.stream))

  private def killEff(state: ProcessState, signum: SignalNumber): EmIO[EmileError.IO, Unit] =
    EffIO.lift(Routing.onOwner(state.poller)(LiveHandle.tryUse(state.live, closedKill)(handle => doKill(handle, signum))))

  private def doKill(handle: Ptr[Byte], signum: SignalNumber): Either[EmileError.IO, Unit] =
    val rc = LibUV.uv_process_kill(handle, SignalNumber.unwrap(signum))
    if rc == 0 then Right(())
    else if rc == ErrorCode.UV_ESRCH then Left(EmileError.IO.AlreadyClosed)
    else Left(IOMapping.fromCode(rc))

  private val closedKill: Either[EmileError.IO, Unit] = Left(EmileError.IO.AlreadyClosed)

  private[emile] def killDetached(pid: Int, signum: SignalNumber): EmIO[EmileError.IO, Unit] =
    EffIO.delay {
      val rc = LibUV.uv_kill(pid, SignalNumber.unwrap(signum))
      if rc == 0 then Right(())
      else if rc == ErrorCode.UV_ESRCH then Left(EmileError.IO.AlreadyClosed)
      else Left(IOMapping.fromCode(rc))
    }

  private def statusEff(state: ProcessState): EmIO[Nothing, ExitStatus] = EffIO.liftF(awaitExit(state))

  private def tryStatusEff(state: ProcessState): EmIO[Nothing, Option[ExitStatus]] =
    EffIO.liftF(Routing.onOwner(state.poller) {
      val current: ExitStatus | Null = state.exit.status
      current.option
    })

  // Awaits the exit on the owning loop thread: complete immediately if the status is already recorded,
  // else register a waiter fired by exit_cb, removing it again if the fibre cancels.
  private def awaitExit(state: ProcessState): IO[ExitStatus] =
    IO.async[ExitStatus]: cb =>
      Routing.onOwner(state.poller):
        val exit = state.exit
        val done: ExitStatus | Null = exit.status
        done.fold[Option[IO[Unit]]] {
          val waiter: ExitStatus => Unit = es => cb(Right(es))
          exit.waiters = waiter :: exit.waiters
          Some(Routing.onOwner(state.poller)(exit.waiters = exit.waiters.filterNot(_ eq waiter)))
        } { value =>
          cb(Right(value))
          None
        }

  // Resource release: apply the OnExit policy, then close every parent pipe end and the handle. The
  // handle close is unconditional - the policy path only governs how the child is brought to exit.
  private def release(state: ProcessState): IO[Unit] =
    applyPolicy(state).flatMap(_ => closeAll(state))

  private def applyPolicy(state: ProcessState): IO[Unit] = state.exitPolicy match
    case OnExit.Await => reap(state)
    case OnExit.Kill => signalProcess(state, signal.SIGKILL).flatMap(_ => reap(state))
    case OnExit.Term(grace) => signalProcess(state, signal.SIGTERM).flatMap(_ => reapWithin(state, grace))
    case OnExit.TermThenKill(grace) =>
      signalProcess(state, signal.SIGTERM).flatMap(_ =>
        reapedWithin(state, grace).flatMap {
          case true => IO.unit
          case false => signalProcess(state, signal.SIGKILL).flatMap(_ => reap(state))
        }
      )

  private def signalProcess(state: ProcessState, signum: Int): IO[Unit] =
    Routing.onOwner(state.poller)(LiveHandle.tryUse(state.live, ())(handle => LibUV.uv_process_kill(handle, signum): Unit))

  private def reap(state: ProcessState): IO[Unit] = awaitExit(state).void

  private def reapWithin(state: ProcessState, grace: FiniteDuration): IO[Unit] =
    awaitExit(state).void.timeoutTo(grace, IO.unit)

  private def reapedWithin(state: ProcessState, grace: FiniteDuration): IO[Boolean] =
    awaitExit(state).as(true).timeoutTo(grace, IO.pure(false))

  private def closeAll(state: ProcessState): IO[Unit] =
    state.pipeStates
      .foldLeft(IO.unit)((io, s) => io.flatMap(_ => StreamCore.release(s)))
      .flatMap(_ => LiveHandle.closeOnOwner(state.live))

  // Reject before libuv: an empty executable path, or a detached spawn with any piped stdio or extra
  // descriptor whose parent pipe end nothing in the returned Detached handle could own.
  private def validate(config: ProcessConfig[?, ?, ?], detached: Boolean): Option[EmileError.Spawn] =
    if config.file.isEmpty then Some(EmileError.Spawn.InvalidArgument("executable path is empty"))
    else if detached && hasPiped(config) then
      Some(
        EmileError.Spawn.InvalidArgument("a detached process cannot pipe its standard streams or extra descriptors; inherit or ignore them")
      )
    else None

  private def hasPiped(config: ProcessConfig[?, ?, ?]): Boolean =
    config.stdinState == Stdio.Piped || config.stdoutState == Stdio.Piped || config.stderrState == Stdio.Piped ||
      config.extraFdList.exists {
        case _: ExtraFd.Piped => true
        case _: ExtraFd.InheritFd => false
      }

  private def acquireManaged(config: ProcessConfig[?, ?, ?]): EmIO[EmileError.Spawn, ProcessState] =
    EffIO.lift(
      validate(config, detached = false) match
        case Some(error) => IO.pure(Left(error))
        case None =>
          LibUVPollingSystem.currentPoller.flatMap(poller =>
            Routing.onOwner(poller)(doSpawnManaged(poller, config)).flatMap {
              case ManagedOutcome.Spawned(state) => IO.pure(Right(state))
              case ManagedOutcome.Failed(error, pipes, process) => closeFailedHandles(poller, pipes, process).as(Left(error))
            }
          )
    )

  private def acquireDetached(config: ProcessConfig[?, ?, ?]): EmIO[EmileError.Spawn, Detached] =
    EffIO.lift(
      validate(config, detached = true) match
        case Some(error) => IO.pure(Left(error))
        case None =>
          LibUVPollingSystem.currentPoller.flatMap(poller =>
            Routing.onOwner(poller)(doSpawnDetached(poller, config)).flatMap {
              case DetachedOutcome.Spawned(detached) => IO.pure(Right(detached))
              case DetachedOutcome.Failed(error, pipes, process) => closeFailedHandles(poller, pipes, process).as(Left(error))
            }
          )
    )

  private enum ManagedOutcome:
    case Spawned(state: ProcessState)
    case Failed(error: EmileError.Spawn, pipes: List[Ptr[Byte]], process: Ptr[Byte])

  private enum DetachedOutcome:
    case Spawned(detached: Detached)
    case Failed(error: EmileError.Spawn, pipes: List[Ptr[Byte]], process: Ptr[Byte])

  private enum ExtraSlot:
    case Piped(childFd: Int, direction: PipeDirection, handle: Ptr[Byte])
    case Inherited(childFd: Int, fd: Int)

  final private class PipePlan(
    val stdinHandle: Ptr[Byte] | Null,
    val stdoutHandle: Ptr[Byte] | Null,
    val stderrHandle: Ptr[Byte] | Null,
    val extras: List[ExtraSlot],
    val allHandles: List[Ptr[Byte]]
  )

  // Marshalling and the raw libuv calls: handle calloc null-checks, the options / stdio / argv / env
  // pointer arithmetic, the union fd write, and the null-pointer sentinels for inherited env / cwd.
  // scalafix:off DisableSyntax

  private def doSpawnManaged(poller: LibUVPoller, config: ProcessConfig[?, ?, ?]): ManagedOutcome =
    val process = allocProcessHandle()
    buildPipes(poller, config) match
      case Left((error, handles)) => ManagedOutcome.Failed(error, handles, process)
      case Right(plan) =>
        val rc = Zone.acquire(z => marshalAndSpawn(poller, process, config, plan, detached = false, exitCb)(using z))
        if rc < 0 then ManagedOutcome.Failed(SpawnMapping.fromCode(rc), plan.allHandles, process)
        else ManagedOutcome.Spawned(finishManaged(poller, process, config, plan))

  private def doSpawnDetached(poller: LibUVPoller, config: ProcessConfig[?, ?, ?]): DetachedOutcome =
    val process = allocProcessHandle()
    buildPipes(poller, config) match
      case Left((error, handles)) => DetachedOutcome.Failed(error, handles, process)
      case Right(plan) =>
        val rc = Zone.acquire(z => marshalAndSpawn(poller, process, config, plan, detached = true, detachedExitCb)(using z))
        if rc < 0 then DetachedOutcome.Failed(SpawnMapping.fromCode(rc), plan.allHandles, process)
        else
          CallbackBridge.store(poller, process, new DetachedExit(poller))
          LibUV.uv_unref(process)
          DetachedOutcome.Spawned(Detached.wrap(LibUV.uv_process_get_pid(process)))

  // Allocate and initialise a parent uv_pipe_t for every piped slot; on an init failure free the
  // just-allocated handle and break with the already-initialised handles for the caller to close.
  private def buildPipes(poller: LibUVPoller, config: ProcessConfig[?, ?, ?]): Either[(EmileError.Spawn, List[Ptr[Byte]]), PipePlan] =
    boundary:
      val created = ListBuffer.empty[Ptr[Byte]]
      def pipe(): Ptr[Byte] =
        val handle = allocPipeHandle()
        val rc = LibUV.uv_pipe_init(poller.loop, handle, 0)
        if rc != 0 then
          stdlib.free(handle)
          boundary.break(Left((SpawnMapping.fromCode(rc), created.toList)))
        (created += handle): Unit
        handle
      val stdinH: Ptr[Byte] | Null = if config.stdinState == Stdio.Piped then pipe() else null
      val stdoutH: Ptr[Byte] | Null = if config.stdoutState == Stdio.Piped then pipe() else null
      val stderrH: Ptr[Byte] | Null = if config.stderrState == Stdio.Piped then pipe() else null
      val extras = config.extraFdList.zipWithIndex.map { (extra, index) =>
        val childFd = 3 + index
        extra match
          case ExtraFd.Piped(direction) => ExtraSlot.Piped(childFd, direction, pipe())
          case ExtraFd.InheritFd(fd) => ExtraSlot.Inherited(childFd, fd)
      }
      Right(new PipePlan(stdinH, stdoutH, stderrH, extras, created.toList))

  private def finishManaged(poller: LibUVPoller, process: Ptr[Byte], config: ProcessConfig[?, ?, ?], plan: PipePlan): ProcessState =
    val stdinHandle: Ptr[Byte] | Null = plan.stdinHandle
    val stdoutHandle: Ptr[Byte] | Null = plan.stdoutHandle
    val stderrHandle: Ptr[Byte] | Null = plan.stderrHandle
    val stdinState: StreamState | Null = stdinHandle.fold[StreamState | Null](null)(makeState(poller, _))
    val stdoutState: StreamState | Null = stdoutHandle.fold[StreamState | Null](null)(makeState(poller, _))
    val stderrState: StreamState | Null = stderrHandle.fold[StreamState | Null](null)(makeState(poller, _))
    val extraStates: Map[Int, ExtraState] = plan.extras.collect { case ExtraSlot.Piped(childFd, direction, handle) =>
      childFd -> new ExtraState(makeState(poller, handle), direction)
    }.toMap
    val pipeStates = List(stdinState, stdoutState, stderrState).flatMap(_.option) ++ extraStates.values.iterator.map(_.stream).toList
    val exit = new ProcessExit
    CallbackBridge.store(poller, process, exit)
    val pid = LibUV.uv_process_get_pid(process)
    new ProcessState(
      LiveHandle(poller, process),
      poller,
      pid,
      config.exitPolicy,
      exit,
      stdinState,
      stdoutState,
      stderrState,
      extraStates,
      pipeStates
    )
  end finishManaged

  private def makeState(poller: LibUVPoller, handle: Ptr[Byte]): StreamState =
    new StreamState(LiveHandle(poller, handle), ResizableBuffer(StreamCore.DefaultReadSize))

  private def marshalAndSpawn(
    poller: LibUVPoller,
    process: Ptr[Byte],
    config: ProcessConfig[?, ?, ?],
    plan: PipePlan,
    detached: Boolean,
    exitCallback: LibUV.ExitCB
  )(using z: Zone): Int =
    val fileC = toCString(config.file)
    val argvCount = config.args.length + 2
    val argv = z.alloc(sizeof[CString] * argvCount.toCSize).asInstanceOf[Ptr[CString]]
    argv(0) = fileC
    config.args.iterator.zipWithIndex.foreach((arg, i) => argv(i + 1) = toCString(arg))
    argv(argvCount - 1) = nullPtr[CChar]
    val envPtr: Ptr[CString] = config.envSetting match
      case Env.Inherit => nullPtr[CString]
      case Env.Replace(vars) => buildEnv(vars)
      case Env.Extend(overrides) => buildEnv(sys.env ++ overrides)
    val cwdC: CString = config.cwdPath.fold(nullPtr[CChar])(path => toCString(path.toString))
    val stdioCount = 3 + config.extraFdList.length
    val stdio = z.alloc(sizeof[LibUV.StdioContainer] * stdioCount.toCSize).asInstanceOf[Ptr[LibUV.StdioContainer]]
    setStdio(stdio + 0, config.stdinState, plan.stdinHandle, 0, childReads = true)
    setStdio(stdio + 1, config.stdoutState, plan.stdoutHandle, 1, childReads = false)
    setStdio(stdio + 2, config.stderrState, plan.stderrHandle, 2, childReads = false)
    plan.extras.foreach(slot => setExtra(stdio, slot))
    val (uid, gid, credFlags) = config.creds match
      case Some((u, g)) => (u, g, LibUV.UV_PROCESS_SETUID | LibUV.UV_PROCESS_SETGID)
      case None => (0, 0, 0)
    val flags = credFlags | (if detached then LibUV.UV_PROCESS_DETACHED else 0)
    val opts = z.alloc(sizeof[LibUV.ProcessOptions]).asInstanceOf[Ptr[LibUV.ProcessOptions]]
    opts._1 = exitCallback
    opts._2 = fileC
    opts._3 = argv
    opts._4 = envPtr
    opts._5 = cwdC
    opts._6 = flags.toUInt
    opts._7 = stdioCount
    opts._8 = stdio
    opts._9 = uid.toUInt
    opts._10 = gid.toUInt
    LibUV.uv_spawn(poller.loop, process, opts)
  end marshalAndSpawn

  private def setStdio(
    container: Ptr[LibUV.StdioContainer],
    state: Stdio,
    handle: Ptr[Byte] | Null,
    inheritFd: Int,
    childReads: Boolean): Unit =
    state match
      case Stdio.Piped =>
        val directionFlag = if childReads then LibUV.UV_READABLE_PIPE else LibUV.UV_WRITABLE_PIPE
        container._1 = LibUV.UV_CREATE_PIPE | directionFlag
        container._2 = handle.unsafe("a piped stream is missing its pipe handle")
      case Stdio.Inherited =>
        container._1 = LibUV.UV_INHERIT_FD
        !container.at2.asInstanceOf[Ptr[CInt]] = inheritFd
      case Stdio.Ignored =>
        container._1 = LibUV.UV_IGNORE
        container._2 = nullPtr[Byte]

  private def setExtra(stdio: Ptr[LibUV.StdioContainer], slot: ExtraSlot): Unit = slot match
    case ExtraSlot.Piped(childFd, direction, handle) =>
      val container = stdio + childFd
      val directionFlags = direction match
        case PipeDirection.Read => LibUV.UV_READABLE_PIPE
        case PipeDirection.Write => LibUV.UV_WRITABLE_PIPE
        case PipeDirection.Duplex => LibUV.UV_READABLE_PIPE | LibUV.UV_WRITABLE_PIPE
      container._1 = LibUV.UV_CREATE_PIPE | directionFlags
      container._2 = handle
    case ExtraSlot.Inherited(childFd, fd) =>
      val container = stdio + childFd
      container._1 = LibUV.UV_INHERIT_FD
      !container.at2.asInstanceOf[Ptr[CInt]] = fd

  private def buildEnv(vars: Map[String, String])(using z: Zone): Ptr[CString] =
    val entries = vars.iterator.map((k, v) => s"$k=$v").toArray
    val arr = z.alloc(sizeof[CString] * (entries.length + 1).toCSize).asInstanceOf[Ptr[CString]]
    var i = 0
    while i < entries.length do
      arr(i) = toCString(entries(i))
      i += 1
    arr(entries.length) = nullPtr[CChar]
    arr

  private def nullPtr[T]: Ptr[T] = fromRawPtr(Intrinsics.castLongToRawPtr(0L))

  private def allocProcessHandle(): Ptr[Byte] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_PROCESS))
    if handle == null then throw new OutOfMemoryError("emile: uv_process_t allocation failed")
    else handle

  private def allocPipeHandle(): Ptr[Byte] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_NAMED_PIPE))
    if handle == null then throw new OutOfMemoryError("emile: uv_pipe_t allocation failed")
    else handle

  // On a spawn failure close every initialised parent pipe, then free the process handle. uv_spawn
  // removes the handle from its loop queue on failure and never starts it, so a bare free is correct.
  private def closeFailedHandles(poller: LibUVPoller, pipes: List[Ptr[Byte]], process: Ptr[Byte]): IO[Unit] =
    pipes
      .foldLeft(IO.unit)((io, handle) => io.flatMap(_ => Routing.closeHandle(poller, handle)))
      .flatMap(_ => Routing.onOwner(poller)(stdlib.free(process)))

  // exit_cb on the owning loop thread: record the child's status and fire every waiter. libuv has
  // already stopped the handle by now, so this never closes it - the resource release does.
  private def completeExit(exit: ProcessExit, status: ExitStatus): Unit =
    exit.status = status
    exit.waiters.foreach(waiter => waiter(status))
    exit.waiters = Nil

  // exit_status carries the 8-bit wait code and term_signal the terminating signal; they are mutually
  // exclusive, so a non-zero signal means the child was signalled. wrap, not apply: waitpid always
  // yields a valid signal number, and a throw must never cross the C callback boundary.
  private def decodeStatus(exitStatus: Long, termSignal: Int): ExitStatus =
    if termSignal != 0 then ExitStatus.Signaled(SignalNumber.wrap(termSignal))
    else ExitStatus.Exited(exitStatus.toInt)

  private val exitCb: LibUV.ExitCB = (handle: Ptr[Byte], exitStatus: CLongLong, termSignal: CInt) =>
    completeExit(CallbackBridge.load[ProcessExit](handle), decodeStatus(exitStatus, termSignal))

  // A detached child self-closes on exit: libuv reaps it, then this frees the unreferenced handle.
  private val detachedExitCb: LibUV.ExitCB = (handle: Ptr[Byte], _: CLongLong, _: CInt) =>
    if LibUV.uv_is_closing(handle) == 0 then LibUV.uv_close(handle, detachedCloseCb)

  private val detachedCloseCb: LibUV.CloseCB = (handle: Ptr[Byte]) =>
    val holder = CallbackBridge.load[DetachedExit](handle)
    CallbackBridge.clear(holder.poller, handle)
    stdlib.free(handle)

  // scalafix:on DisableSyntax

end Process
