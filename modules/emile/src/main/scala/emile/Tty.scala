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

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Stream

import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing
import emile.unsafe.TtyRawRestore

/** The kind of a file descriptor, as `uv_guess_handle` classifies it: a terminal, a pipe (a named
  * pipe or a Unix-domain socket), a file (a regular file or a character device), a TCP or UDP
  * socket, or none of these. Reported by [[Tty.guess]].
  */
enum FdKind derives CanEqual:
  case Tty, Pipe, File, Tcp, Udp, Unknown

/** A terminal window size in character cells. Read through [[Tty.size]] and streamed by
  * [[Tty.resizes]]; a pseudo-terminal whose controller never set dimensions legitimately reads
  * `WinSize(0, 0)`.
  */
final case class WinSize(cols: Int, rows: Int)

/** Equality for [[WinSize]]. */
object WinSize:
  given CanEqual[WinSize, WinSize] = CanEqual.derived

/** A terminal handle, backed by a `uv_tty_t`. Acquired through [[Tty$ Tty]] over a terminal file
  * descriptor; a non-terminal descriptor is rejected at [[Tty.open]]. It shares the byte-stream
  * surface of a [[Socket$ Socket]] - [[Tty.reads]], [[Tty.read]], [[Tty.consume]], [[Tty.write]],
  * [[Tty.writes]] - and adds terminal-only operations: [[Tty.raw]] mode with a crash-safe restore,
  * the window [[Tty.size]], and the [[Tty.resizes]] stream. Escape-sequence parsing (keys, mouse,
  * paste) is above emile.
  */
opaque type Tty = StreamState

/** Terminal detection, the [[Tty]] resource, and its terminal-only and byte-stream operations. */
object Tty:

  /** Whether `fd` refers to a terminal - the `isatty` short-circuit of `uv_guess_handle`. Loop-free
    * and infallible.
    */
  def isTty(fd: Int): Boolean = LibUV.uv_guess_handle(fd) == LibUV.UV_TTY

  /** Classify `fd` without opening it - the honest mapping of `uv_guess_handle`'s decision tree. A
    * character device (for example `/dev/null`) reads as [[FdKind.File]], and a Unix-domain socket
    * as [[FdKind.Pipe]], per libuv. Loop-free and infallible.
    */
  def guess(fd: Int): FdKind = fromGuess(LibUV.uv_guess_handle(fd))

  /** A terminal handle over `fd`. libuv reopens the terminal by path and owns a non-blocking
    * duplicate, so the caller's descriptor is unaffected. A non-terminal `fd` fails with
    * [[EmileError.IO.InvalidArgument]].
    */
  def open(fd: Int): EmResource[EmileError.IO, Tty] =
    Resource.make[EffIO.Of[EmileError.IO], Tty](openAcquire(fd))(tty => EffIO.liftF(release(tty)))

  given CanEqual[Tty, Tty] = CanEqual.derived

  extension (tty: Tty)

    /** Raw mode for the terminal's lifetime as the resource is held: input is delivered unbuffered,
      * uninterpreted, and unechoed (`uv_tty_set_mode` with the raw VT mode), restored to cooked
      * mode on release. A second concurrent `raw` acquisition - on this or any terminal - fails
      * with [[EmileError.IO.ConflictingOperation]]: the crash-safe restore rests on a single
      * process-global raw terminal.
      *
      * The restore holds across every exit path, so a dying program never leaves the shell in raw
      * mode. Orderly exit and cancellation are covered by the resource release; the five fatal
      * signals (`SIGSEGV`, `SIGBUS`, `SIGILL`, `SIGFPE`, `SIGABRT`) by handlers installed only for
      * the raw window, which restore the terminal then re-raise so the original crash still
      * reports; and a non-signal hard exit by a shutdown hook. Signals a caller owns through
      * [[Signal$ Signal]] - `SIGINT`, `SIGTERM`, `SIGTSTP` - are untouched.
      */
    def raw: EmResource[EmileError.IO, Unit] =
      Resource.make[EffIO.Of[EmileError.IO], Unit](rawAcquire(tty))(_ => EffIO.liftF(rawRelease(tty)))

    /** The current window size. `WinSize(0, 0)` is a legitimate reading on a pseudo-terminal whose
      * controller never set dimensions - fall back to a default where a real size is required.
      */
    def size: EmIO[EmileError.IO, WinSize] = winSize(tty)

    /** The window size now, then a fresh reading on each `SIGWINCH` - the resize feed for a
      * terminal UI, over the shared [[Signal$ Signal]] machinery.
      */
    def resizes: EmStream[EmileError.IO, WinSize] =
      Stream.eval[EmIO.Of[EmileError.IO], WinSize](winSize(tty)) ++
        Signal.watch(SignalNumber.SIGWINCH).evalMap(_ => winSize(tty))

    /** A back-pressured byte stream over a persistent libuv read - terminal input, byte by byte. */
    def reads: EmStream[EmileError.IO, Byte] = StreamCore.reads(tty)

    /** Reads one chunk and hands `f` a borrowed [[boilerplate.Slice Slice]] over the receive
      * buffer, sparing a copy. The slice is valid only while `f` runs, as the next read reuses the
      * buffer, so `f` must not retain it. `None` at end of input.
      */
    inline def read[E <: Throwable, A](f: Slice => EmIO[E, A]): EmIO[EmileError.IO | E, Option[A]] =
      StreamCore.readPtrOnce(tty, f)

    /** Reads continuously, running `onChunk` inline on the owning loop thread with a borrowed
      * [[boilerplate.Slice Slice]] over each chunk until end of input. `onChunk` must neither block
      * nor retain its slice past returning; a `Left(e)` stops the read early.
      */
    inline def consume[E <: Throwable](onChunk: Slice => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
      StreamCore.consumeAll(tty, onChunk)

    /** Write `slice` with no copy. The region is borrowed by the write until the effect completes,
      * so do not mutate the written range while it is in flight.
      */
    inline def write(slice: Slice): EmIO[EmileError.IO, Unit] = StreamCore.writeSlice(tty, slice)

    /** A pipe that writes every byte the source emits to the terminal, chunk-by-chunk. */
    def writes: EmPipe[EmileError.IO, Byte, Nothing] = StreamCore.writes(tty)

  end extension

  private def fromGuess(code: Int): FdKind = code match
    case LibUV.UV_TTY => FdKind.Tty
    case LibUV.UV_NAMED_PIPE => FdKind.Pipe
    case LibUV.UV_FILE => FdKind.File
    case LibUV.UV_TCP => FdKind.Tcp
    case LibUV.UV_UDP => FdKind.Udp
    case _ => FdKind.Unknown

  private def poller(tty: Tty): LibUVPoller = LiveHandle.poller(tty.live)

  private def openAcquire(fd: Int): EmIO[EmileError.IO, Tty] =
    EffIO.lift(
      for
        poller <- LibUVPollingSystem.currentPoller
        handle <- IO(allocHandle())
        result <- Routing.onOwner(poller)(ttyInstall(poller, handle, fd))
      yield result
    )

  private def release(tty: Tty): IO[Unit] = StreamCore.release(tty)

  private def winSize(tty: Tty): EmIO[EmileError.IO, WinSize] =
    EffIO.lift(Routing.onOwner(poller(tty))(LiveHandle.tryUse(tty.live, closedWinSize)(readWinSize)))

  private val closedWinSize: Either[EmileError.IO, WinSize] = Left(EmileError.IO.AlreadyClosed)

  private def rawAcquire(tty: Tty): EmIO[EmileError.IO, Unit] =
    EffIO.lift(
      Routing.onOwner(poller(tty)):
        if !TtyRawRestore.install() then Left(EmileError.IO.ConflictingOperation)
        else
          // If already installed but the handle is gone (raw is nested in the tty's scope, so this is
          // defensive), undo the install so the guard and handlers are not stranded.
          LiveHandle.tryUse(tty.live, rawClosedUndo()): handle =>
            val rc = LibUV.uv_tty_set_mode(handle, LibUV.UV_TTY_MODE_RAW_VT)
            if rc < 0 then
              TtyRawRestore.uninstall()
              Left(IOMapping.fromCode(rc))
            else Right(())
    )

  private def rawClosedUndo(): Either[EmileError.IO, Unit] =
    TtyRawRestore.uninstall()
    Left(EmileError.IO.AlreadyClosed)

  private def rawRelease(tty: Tty): IO[Unit] =
    Routing.onOwner(poller(tty)):
      LiveHandle.tryUse(tty.live, ())(handle => LibUV.uv_tty_set_mode(handle, LibUV.UV_TTY_MODE_NORMAL): Unit)
      TtyRawRestore.uninstall()

  // FFI: handle calloc null-check and a stackalloc winsize pair.
  // scalafix:off DisableSyntax

  private def ttyInstall(poller: LibUVPoller, handle: Ptr[Byte], fd: Int): Either[EmileError.IO, Tty] =
    val rc = LibUV.uv_tty_init(poller.loop, handle, fd, 0)
    if rc != 0 then
      stdlib.free(handle)
      Left(initError(rc, fd))
    else Right(new StreamState(LiveHandle(poller, handle), ResizableBuffer(StreamCore.DefaultReadSize)))

  // A non-terminal fd is UV_EINVAL, surfaced with the offending descriptor rather than a bare code.
  private def initError(rc: Int, fd: Int): EmileError.IO =
    if rc == ErrorCode.UV_EINVAL then EmileError.IO.InvalidArgument(s"file descriptor $fd is not a terminal")
    else IOMapping.fromCode(rc)

  private def readWinSize(handle: Ptr[Byte]): Either[EmileError.IO, WinSize] =
    val width = stackalloc[CInt]()
    val height = stackalloc[CInt]()
    val rc = LibUV.uv_tty_get_winsize(handle, width, height)
    if rc < 0 then Left(IOMapping.fromCode(rc))
    else Right(WinSize(!width, !height))

  private def allocHandle(): Ptr[Byte] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(LibUV.UV_TTY))
    if handle == null then throw new OutOfMemoryError("emile: uv_tty_t allocation failed")
    else handle

  // scalafix:on DisableSyntax

end Tty
