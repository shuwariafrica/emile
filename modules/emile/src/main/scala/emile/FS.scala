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
import scala.concurrent.duration.FiniteDuration
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.CInt
import scala.scalanative.unsafe.CString
import scala.scalanative.unsafe.CUnsignedInt
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.fromCString
import scala.scalanative.unsafe.sizeof
import scala.scalanative.unsafe.toCString
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.unsafe.UnboundedQueue
import fs2.Stream

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.Routing

/** The kind of change reported on a watched path: `Renamed` for an entry created, deleted, moved,
  * or renamed; `Changed` for an entry's contents or attributes (the platform reports nothing
  * finer).
  */
enum FSChange derives CanEqual:
  case Renamed, Changed

/** A filesystem change: the kinds of change in one notification, and the affected entry's name
  * relative to the watched path when the platform supplies one (absent when it does not, e.g. for a
  * watched file reported without a name, or from [[FS$ FS]].poll).
  */
final case class FSEvent(changes: Set[FSChange], filename: Option[String]) derives CanEqual

final private class FSState(
  val handle: Ptr[Byte],
  val poller: LibUVPoller,
  val eventsQueue: UnboundedQueue[IO, Either[EmileError.IO, FSEvent]],
  val changesQueue: UnboundedQueue[IO, Either[EmileError.IO, Unit]]
):
  // Single-consumer guards: events and changes each drain one queue, so two concurrent consumers would
  // split the stream between them. Owner-confined - set and cleared only inside Routing.onOwner.
  var eventsActive: Boolean = false // scalafix:ok DisableSyntax.var
  var changesActive: Boolean = false // scalafix:ok DisableSyntax.var
  // Coalescing latch for changes: the callback offers a pulse only when none is outstanding, and the
  // changes stream clears it on take - so the queue holds at most one pending pulse. A latch over an
  // unbounded queue rather than a bounded one because the smallest bounded queue is capacity two and a
  // full one drops the terminal error; the latch coalesces to exactly one and the error always gets
  // through. Loop-thread-confined: the callback and the clear (via Routing.onOwner) both run there.
  var changePending: Boolean = false // scalafix:ok DisableSyntax.var
end FSState

/** A filesystem-change watcher over a path, acquired through [[FS$ FS]].watch (inotify, via
  * `uv_fs_event_t`) or [[FS$ FS]].poll (stat-polling, via `uv_fs_poll_t`). The same changes surface
  * two ways: `events` reports every change, `changes` is a coalesced re-scan pulse for high churn.
  * Each is drained by a single subscriber.
  */
opaque type FS = FSState

/** Watch construction, the change streams, and equality for [[FS]]. */
object FS:

  /** Watches `path` for changes via inotify (`uv_fs_event_t`) for the resource's lifetime. The
    * watcher is created on - and closed back on - the loop of the worker the resource is acquired
    * on. Prefer watching a directory over a single file: a rename that replaces a watched file
    * detaches the underlying inotify watch, after which it stops reporting, whereas a directory
    * watch keeps reporting its entries' changes. For a path inotify cannot serve (some network or
    * container filesystems) use [[poll]].
    */
  def watch(path: Path): EmResource[EmileError.IO, FS] =
    Resource.make[EffIO.Of[EmileError.IO], FS](
      acquire(LibUV.UV_FS_EVENT)((poller, handle) => LibUV.uv_fs_event_init(poller.loop, handle)) { (_, handle) =>
        Zone(LibUV.uv_fs_event_start(handle, fsEventCb, toCString(path.toString), 0.toUInt))
      }
    )(release)

  /** Watches `path` by polling its stat every `interval` (`uv_fs_poll_t`), for backends inotify
    * cannot serve - network filesystems, some container mounts. A change surfaces only on a stat
    * transition: the path appearing or its size or mtime changing is a [[FSChange.Changed]], it
    * becoming inaccessible is a [[FSChange.Renamed]]; no entry name is reported. Polling a
    * directory reflects only the directory's own stat (its entries added or removed), not changes
    * within its files, so it is coarser than [[watch]] - prefer `watch` where inotify works. Each
    * interval costs a `stat`, so choose a period of seconds; a zero or sub-millisecond interval
    * polls every millisecond.
    */
  def poll(path: Path, interval: FiniteDuration): EmResource[EmileError.IO, FS] =
    Resource.make[EffIO.Of[EmileError.IO], FS](
      acquire(LibUV.UV_FS_POLL)((poller, handle) => LibUV.uv_fs_poll_init(poller.loop, handle)) { (_, handle) =>
        Zone(LibUV.uv_fs_poll_start(handle, fsPollCb, toCString(path.toString), intervalMillis(interval)))
      }
    )(release)

  given CanEqual[FS, FS] = CanEqual.derived

  extension (fs: FS)
    /** Every change observed on the watched path, in arrival order, until the resource releases -
      * the lossless view. A libuv watch error ends the stream on the [[EmileError.IO]] channel. The
      * platform coalesces rapid changes and may report a change with no entry name, so debouncing
      * is the consumer's concern. For sustained high churn prefer [[changes]], which cannot grow
      * unbounded. Drained by one subscriber: a second concurrent `events` fails fast with
      * [[EmileError.IO.ConflictingOperation]].
      */
    def events: EmStream[EmileError.IO, FSEvent] =
      Stream
        .resource(consumer(fs, () => fs.eventsActive, active => fs.eventsActive = active))
        .flatMap(_ => Stream.repeatEval[EmIO.Of[EmileError.IO], FSEvent](EffIO.lift(fs.eventsQueue.take)))

    /** A coalesced re-scan pulse - one `()` per burst of changes - for the reload-on-change
      * workflow (re-read the path and reconcile, where the individual events do not matter). At
      * most one pulse is outstanding, so it cannot grow unbounded and survives a kernel-queue
      * overflow: the right view for high churn. A watch error ends the stream on the
      * [[EmileError.IO]] channel. Drained by one subscriber.
      */
    def changes: EmStream[EmileError.IO, Unit] =
      Stream
        .resource(consumer(fs, () => fs.changesActive, active => fs.changesActive = active))
        .flatMap(_ => Stream.repeatEval[EmIO.Of[EmileError.IO], Unit](changesPull(fs)))
  end extension

  /** The full [[FileStatus]] of `path`, following a final symbolic link (`stat`). */
  def stat(path: Path): EmIO[EmileError.IO, FileStatus] =
    pathOp(statusResult)((poller, req) => Zone(LibUV.uv_fs_stat(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** The full [[FileStatus]] of `path` itself, not following a final symbolic link (`lstat`). */
  def lstat(path: Path): EmIO[EmileError.IO, FileStatus] =
    pathOp(statusResult)((poller, req) => Zone(LibUV.uv_fs_lstat(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** The [[FileSystemInfo]] of the filesystem holding `path` (`statfs`). */
  def statfs(path: Path): EmIO[EmileError.IO, FileSystemInfo] =
    pathOp(statfsResult)((poller, req) => Zone(LibUV.uv_fs_statfs(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** The canonical absolute form of `path`, resolving every symbolic link (`realpath`). */
  def realpath(path: Path): EmIO[EmileError.IO, Path] =
    pathOp(pathResult)((poller, req) => Zone(LibUV.uv_fs_realpath(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** Whether `path` is accessible to the calling process for `mode` - `false` also for a path that
    * does not exist or is not permitted, so the everyday question answers without an exception; any
    * other failure stays typed.
    */
  def access(path: Path, mode: FileAccess): EmIO[EmileError.IO, Boolean] =
    pathOp(accessResult)((poller, req) =>
      Zone(LibUV.uv_fs_access(poller.loop, req, toCString(path.toString), FileAccess.mode(mode), fsDeliveryCb))
    )

  /** Whether `path` exists - the [[access]] existence check. */
  def exists(path: Path): EmIO[EmileError.IO, Boolean] = access(path, FileAccess.Exists)

  /** Sets the permission bits of `path` to `permissions` (an octal mode) via `chmod`. */
  def chmod(path: Path, permissions: Int): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) => Zone(LibUV.uv_fs_chmod(poller.loop, req, toCString(path.toString), permissions, fsDeliveryCb)))

  /** Sets the owner and group of `path` via `chown`, following a final symbolic link; either id may
    * be `-1` to leave it unchanged.
    */
  def chown(path: Path, uid: Int, gid: Int): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(LibUV.uv_fs_chown(poller.loop, req, toCString(path.toString), uid.toUInt, gid.toUInt, fsDeliveryCb))
    )

  /** Sets the owner and group of `path` itself via `lchown`, not following a final symbolic link. */
  def lchown(path: Path, uid: Int, gid: Int): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(LibUV.uv_fs_lchown(poller.loop, req, toCString(path.toString), uid.toUInt, gid.toUInt, fsDeliveryCb))
    )

  /** Sets the access and modification times of `path` via `utime`; each [[SetTime]] is set
    * explicitly, to now, or left unchanged independently.
    */
  def setTimes(path: Path, atime: SetTime, mtime: SetTime): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(
        LibUV.uv_fs_utime(poller.loop, req, toCString(path.toString), SetTime.toDouble(atime), SetTime.toDouble(mtime), fsDeliveryCb)
      )
    )

  /** Removes the file or symbolic link `path` (`unlink`); a directory fails with
    * [[EmileError.IO.IsADirectory]] - use [[rmdir]].
    */
  def unlink(path: Path): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) => Zone(LibUV.uv_fs_unlink(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** Renames `from` to `to` (`rename`), replacing an existing `to` atomically. */
  def rename(from: Path, to: Path): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(LibUV.uv_fs_rename(poller.loop, req, toCString(from.toString), toCString(to.toString), fsDeliveryCb))
    )

  /** Copies `from` to `to`, overwriting an existing `to` with a plain byte copy. */
  def copy(from: Path, to: Path): EmIO[EmileError.IO, Unit] = copyWith(from, to, 0)

  /** Copies `from` to `to` per `options` - whether to overwrite, and the [[ReflinkMode]]. On a
    * mid-copy failure the partial destination is removed, so it briefly exists then vanishes.
    */
  def copy(from: Path, to: Path, options: CopyOptions): EmIO[EmileError.IO, Unit] =
    copyWith(from, to, CopyOptions.flags(options))

  /** Creates a hard link `to` referring to the same inode as `from` (`link`). */
  def link(from: Path, to: Path): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(LibUV.uv_fs_link(poller.loop, req, toCString(from.toString), toCString(to.toString), fsDeliveryCb))
    )

  /** Creates a symbolic link at `linkPath` pointing to `target` (`symlink`); `target` is stored
    * verbatim, so a relative target resolves against `linkPath`'s directory.
    */
  def symlink(target: Path, linkPath: Path): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(LibUV.uv_fs_symlink(poller.loop, req, toCString(target.toString), toCString(linkPath.toString), 0, fsDeliveryCb))
    )

  /** The target a symbolic link `path` points to, verbatim as stored (`readlink`). */
  def readlink(path: Path): EmIO[EmileError.IO, Path] =
    pathOp(pathResult)((poller, req) => Zone(LibUV.uv_fs_readlink(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** Creates directory `path` with permission `0777` (masked by the process umask). */
  def mkdir(path: Path): EmIO[EmileError.IO, Unit] = mkdir(path, DefaultDirPermissions)

  /** Creates directory `path` with `permissions` (an octal mode, masked by the process umask). */
  def mkdir(path: Path, permissions: Int): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) => Zone(LibUV.uv_fs_mkdir(poller.loop, req, toCString(path.toString), permissions, fsDeliveryCb)))

  /** Removes the empty directory `path` (`rmdir`); a non-empty one fails with
    * [[EmileError.IO.DirectoryNotEmpty]].
    */
  def rmdir(path: Path): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) => Zone(LibUV.uv_fs_rmdir(poller.loop, req, toCString(path.toString), fsDeliveryCb)))

  /** Streams the entries of directory `path` - the entry name relative to `path` and its
    * [[DirEntryKind]] - over `opendir`/`readdir`/`closedir`, one bounded batch at a time, so a
    * large directory is traversed in constant memory and with backpressure. `.` and `..` are not
    * emitted.
    */
  def list(path: Path): EmStream[EmileError.IO, DirEntry] =
    Stream
      .resource(listResource(path))
      .flatMap(state =>
        Stream.repeatEval[EmIO.Of[EmileError.IO], Option[List[DirEntry]]](readBatch(state)).unNoneTerminate.flatMap(Stream.emits)
      )

  /** Creates a uniquely named temporary directory from `template` - a path whose final component
    * ends in six `X`s, replaced with random characters (`mkdtemp`) - and yields its path. The
    * directory is not removed on any schedule; deleting it is the caller's, so it survives a
    * write-temp-then-rename beyond the effect's scope.
    */
  def tempDirectory(template: String): EmIO[EmileError.IO, Path] =
    EffIO.attempt(
      LibUVPollingSystem.currentPoller.flatMap(poller =>
        fsRequest(poller, noBuffer)(req => Zone(LibUV.uv_fs_mkdtemp(poller.loop, req, toCString(template), fsDeliveryCb)))(pathAtResult)
      ),
      EmileError.IO.Unexpected(_)
    )

  /** Creates a uniquely named temporary file from `template` (as [[tempDirectory]], via `mkstemp`)
    * and yields a [[TempFile]] - its path and an [[OpenFile]] on the descriptor. The descriptor is
    * closed on release; the file itself persists, for the caller to move into place or delete.
    */
  def tempFile(template: String): EmResource[EmileError.IO, TempFile] =
    Resource.make[EffIO.Of[EmileError.IO], TempFile](acquireTempFile(template))(temp => EffIO.liftF(OpenFile.close(temp.file)))

  // FFI for the path operations: request alloc / cleanup null-checks, req->ptr reinterpretation, and
  // the caller-owned dirents block for the readdir protocol.
  // scalafix:off DisableSyntax

  private val noBuffer: AnyRef = new Object

  // The permission a one-argument mkdir uses (0777 octal), before the process umask is applied.
  private inline val DefaultDirPermissions = 0x1ff

  // Number of dirents read per uv_fs_readdir batch - the streaming granularity for FS.list.
  private inline val ListBatchSize = 64

  // Runs a path operation: acquire the current poller, submit on its loop, and deliver the typed
  // result. The Zone-allocated path CString need only survive the submit (libuv copies it for async).
  private def pathOp[A](interpret: Ptr[Byte] => Either[EmileError.IO, A])(submit: (LibUVPoller, Ptr[Byte]) => Int): EmIO[EmileError.IO, A] =
    EffIO.attempt(
      LibUVPollingSystem.currentPoller.flatMap(poller => fsRequest(poller, noBuffer)(req => submit(poller, req))(interpret)),
      EmileError.IO.Unexpected(_)
    )

  private def copyWith(from: Path, to: Path, flags: Int): EmIO[EmileError.IO, Unit] =
    pathOp(unitResult)((poller, req) =>
      Zone(LibUV.uv_fs_copyfile(poller.loop, req, toCString(from.toString), toCString(to.toString), flags, fsDeliveryCb))
    )

  // Submit an fs op on `poller`'s loop and deliver its typed result. `submit(req)` issues the uv_fs_*
  // call; `interpret(req)` reads the outcome from the completed request, before the cleanup that frees
  // it. `keepAlive` is held reachable (via the anchored delivery) until the callback fires.
  // Cancellation cancels a still-queued op; its callback then delivers UV_ECANCELED and reclaims it.
  private def fsRequest[A](poller: LibUVPoller, keepAlive: AnyRef)(submit: Ptr[Byte] => Int)(
    interpret: Ptr[Byte] => Either[EmileError.IO, A]): IO[A] =
    IO.async[A]: cb =>
      Routing.onOwner(poller):
        val req = allocRequest()
        val rc = submit(req)
        if rc < 0 then
          failRequest(req, cb, rc)
          None
        else
          val run: Ptr[Byte] => Unit = r =>
            val outcome = interpret(r)
            CallbackBridge.releaseReq(poller, r)
            cleanupRequest(r)
            cb(outcome)
          CallbackBridge.storeReq(poller, req, new FsDelivery(run, keepAlive))
          Some(Routing.onOwner(poller)(LibUV.uv_cancel(req): Unit))

  // The anchored completion for an fsRequest: its delivery closure and any borrowed buffer, kept
  // reachable while the request is outstanding.
  final private class FsDelivery(val run: Ptr[Byte] => Unit, @scala.annotation.unused val keepAlive: AnyRef)

  private val fsDeliveryCb: LibUV.FSCB = (req: Ptr[Byte]) => CallbackBridge.loadReq[FsDelivery](req).run(req)

  private def unitResult(req: Ptr[Byte]): Either[EmileError.IO, Unit] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(())

  private def statusResult(req: Ptr[Byte]): Either[EmileError.IO, FileStatus] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(FileStatus.fromStat(LibUV.uv_fs_get_statbuf(req)))

  private def statfsResult(req: Ptr[Byte]): Either[EmileError.IO, FileSystemInfo] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result))
    else Right(FileSystemInfo.fromStatfs(LibUV.uv_fs_get_ptr(req).asInstanceOf[Ptr[LibUV.Statfs]]))

  // realpath / readlink deliver a malloc'd string at req->ptr, freed by the cleanup that follows this
  // read; copy it into an owned Path first.
  private def pathResult(req: Ptr[Byte]): Either[EmileError.IO, Path] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(Path.of(fromCString(LibUV.uv_fs_get_ptr(req))))

  // access answers a question: 0 is yes, and a denied or missing path is a plain "no"; any other code
  // is a genuine typed failure.
  private def accessResult(req: Ptr[Byte]): Either[EmileError.IO, Boolean] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result == 0 then Right(true)
    else if result == ErrorCode.UV_EACCES || result == ErrorCode.UV_ENOENT then Right(false)
    else Left(IOMapping.fromCode(result))

  // mkdtemp / mkstemp deliver the created path at req->path (always allocated, freed by the following
  // cleanup); copy it out first. mkstemp additionally returns the fd at req->result (see acquireTempFile).
  private def pathAtResult(req: Ptr[Byte]): Either[EmileError.IO, Path] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(Path.of(fromCString(LibUV.uv_fs_get_path(req))))

  private def acquireTempFile(template: String): EmIO[EmileError.IO, TempFile] =
    EffIO.attempt(
      LibUVPollingSystem.currentPoller.flatMap(poller =>
        fsRequest(poller, noBuffer)(req => Zone(LibUV.uv_fs_mkstemp(poller.loop, req, toCString(template), fsDeliveryCb)))(req =>
          mkstempResult(poller, req)
        )
      ),
      EmileError.IO.Unexpected(_)
    )

  // mkstemp: fd at req->result, path at req->path - both read before the cleanup that frees the path.
  private def mkstempResult(poller: LibUVPoller, req: Ptr[Byte]): Either[EmileError.IO, TempFile] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result))
    else Right(TempFile(Path.of(fromCString(LibUV.uv_fs_get_path(req))), OpenFile.fromDescriptor(poller, result)))

  // The streaming-list state: the loop, the opendir'd uv_dir_t (C-owned, freed by closedir), and the
  // caller-owned dirents block reused across batches (freed after closedir).
  final private class DirState(val poller: LibUVPoller, val dir: Ptr[Byte], val dirents: Ptr[LibUV.Dirent])

  private def listResource(path: Path): EmResource[EmileError.IO, DirState] =
    Resource.make[EffIO.Of[EmileError.IO], DirState](openDir(path))(closeDir)

  private def openDir(path: Path): EmIO[EmileError.IO, DirState] =
    EffIO.attempt(
      LibUVPollingSystem.currentPoller.flatMap { poller =>
        IO(allocDirents()).flatMap { dirents =>
          fsRequest(poller, noBuffer)(req => Zone(LibUV.uv_fs_opendir(poller.loop, req, toCString(path.toString), fsDeliveryCb)))(
            openDirResult
          ).map(dir => new DirState(poller, dir, dirents))
            // opendir failed: free the dirents block, then re-raise (no dir to close).
            .onError(_ => IO(stdlib.free(dirents)))
        }
      },
      EmileError.IO.Unexpected(_)
    )

  // opendir delivers the uv_dir_t at req->ptr, which the cleanup deliberately does NOT free (closedir
  // does), so it survives past the request.
  private def openDirResult(req: Ptr[Byte]): Either[EmileError.IO, Ptr[Byte]] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(LibUV.uv_fs_get_ptr(req))

  // One readdir batch: point the dir at the reusable dirents block, read up to ListBatchSize entries,
  // then interpret. result 0 ends the stream; the strdup'd names are copied into owned Strings before
  // the following cleanup frees them.
  private def readBatch(state: DirState): EmIO[EmileError.IO, Option[List[DirEntry]]] =
    EffIO.attempt(
      fsRequest(state.poller, noBuffer) { req =>
        val dir = state.dir.asInstanceOf[Ptr[LibUV.Dir]]
        dir._1 = state.dirents
        dir._2 = ListBatchSize.toCSize
        LibUV.uv_fs_readdir(state.poller.loop, req, state.dir, fsDeliveryCb)
      }(req => readBatchResult(state, req)),
      EmileError.IO.Unexpected(_)
    )

  private def readBatchResult(state: DirState, req: Ptr[Byte]): Either[EmileError.IO, Option[List[DirEntry]]] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result))
    else if result == 0 then Right(None)
    else Right(Some(readEntries(state.dirents, result)))

  private def readEntries(dirents: Ptr[LibUV.Dirent], count: Int): List[DirEntry] =
    (0 until count).iterator.map { i =>
      val entry = dirents + i
      DirEntry(fromCString(entry._1), DirEntry.kindOf(entry._2))
    }.toList

  private def closeDir(state: DirState): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(closeDirIO(state).flatMap(_ => IO(stdlib.free(state.dirents))))

  // closedir frees the uv_dir_t; treat any failure as released (nothing left to retry), as file close does.
  private def closeDirIO(state: DirState): IO[Unit] =
    IO.async[Unit]: cb =>
      Routing.onOwner(state.poller):
        val req = allocRequest()
        val rc = LibUV.uv_fs_closedir(state.poller.loop, req, state.dir, fsDeliveryCb)
        if rc < 0 then
          cleanupRequest(req)
          cb(Right(()))
        else
          val run: Ptr[Byte] => Unit = r =>
            CallbackBridge.releaseReq(state.poller, r)
            cleanupRequest(r)
            cb(Right(()))
          CallbackBridge.storeReq(state.poller, req, new FsDelivery(run, noBuffer))
        None

  private def allocDirents(): Ptr[LibUV.Dirent] =
    val block = stdlib.calloc(ListBatchSize.toCSize, sizeof[LibUV.Dirent])
    if block == null then throw new OutOfMemoryError("emile: dirent block allocation failed")
    else block.asInstanceOf[Ptr[LibUV.Dirent]]

  private def allocRequest(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_FS))
    if req == null then throw new OutOfMemoryError("emile: uv_fs_t allocation failed")
    else req

  // Synchronous uv_fs_* failure, before storeReq, so there is no anchor to release - just clean up.
  private def failRequest[A](req: Ptr[Byte], cb: Either[Throwable, A] => Unit, rc: Int): Unit =
    cleanupRequest(req)
    cb(Left(IOMapping.fromCode(rc)))

  private def cleanupRequest(req: Ptr[Byte]): Unit =
    LibUV.uv_fs_req_cleanup(req)
    stdlib.free(req)

  // scalafix:on DisableSyntax

  // The outcome of install, distinguishing the two failure modes by how the handle must be reclaimed: an
  // init failure has already freed the un-init'd handle (uv_close cannot run on it), a start failure
  // leaves the handle init'd and anchored, so acquire closes it through closeHandle.
  private enum Installed:
    case Ready(fs: FS)
    case InitFailed(error: EmileError.IO)
    case StartFailed(error: EmileError.IO)

  // The init-then-start sequence shared by watch and poll; only the libuv backend differs.
  private def acquire(ordinal: Int)(init: (LibUVPoller, Ptr[Byte]) => Int)(
    start: (LibUVPoller, Ptr[Byte]) => Int): EmIO[EmileError.IO, FS] =
    EffIO.lift:
      for
        poller <- LibUVPollingSystem.currentPoller
        eventsQueue <- UnboundedQueue[IO, Either[EmileError.IO, FSEvent]]
        changesQueue <- UnboundedQueue[IO, Either[EmileError.IO, Unit]]
        handle <- IO(allocHandle(ordinal))
        installed <- Routing.onOwner(poller)(install(poller, handle, eventsQueue, changesQueue, init, start))
        result <- installed match
                    case Installed.Ready(fs) => IO.pure(Right(fs))
                    case Installed.InitFailed(error) => IO.pure(Left(error)) // install already freed the un-init'd handle
                    case Installed.StartFailed(error) => Routing.closeHandle(poller, handle).as(Left(error))
      yield result

  private def install(
    poller: LibUVPoller,
    handle: Ptr[Byte],
    eventsQueue: UnboundedQueue[IO, Either[EmileError.IO, FSEvent]],
    changesQueue: UnboundedQueue[IO, Either[EmileError.IO, Unit]],
    init: (LibUVPoller, Ptr[Byte]) => Int,
    start: (LibUVPoller, Ptr[Byte]) => Int
  ): Installed =
    val initRc = init(poller, handle)
    if initRc != 0 then
      stdlib.free(handle)
      Installed.InitFailed(IOMapping.fromCode(initRc))
    else
      val state = new FSState(handle, poller, eventsQueue, changesQueue)
      CallbackBridge.store(poller, handle, state)
      val startRc = start(poller, handle)
      if startRc != 0 then Installed.StartFailed(IOMapping.fromCode(startRc)) else Installed.Ready(state)
  end install

  private def release(fs: FS): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(Routing.closeHandle(fs.poller, fs.handle))

  // Claims a stream's single-consumer slot on the owner thread for its scope, failing fast if taken.
  private def consumer(fs: FSState, taken: () => Boolean, set: Boolean => Unit): EmResource[EmileError.IO, Unit] =
    Resource.make[EffIO.Of[EmileError.IO], Unit](
      EffIO.lift(Routing.onOwner(fs.poller)(if taken() then Left(EmileError.IO.ConflictingOperation) else claim(set)))
    )(_ => EffIO.liftF(Routing.onOwner(fs.poller)(set(false))))

  private def claim(set: Boolean => Unit): Either[EmileError.IO, Unit] =
    set(true)
    Right(())

  // One changes pull: take a pulse, then clear the coalescing latch on the loop thread so the next
  // burst offers again. A Left ends the stream on the typed channel.
  private def changesPull(fs: FSState): EmIO[EmileError.IO, Unit] =
    EffIO.lift(fs.changesQueue.take.flatMap(item => Routing.onOwner(fs.poller)(fs.changePending = false).as(item)))

  private def intervalMillis(interval: FiniteDuration): CUnsignedInt = interval.toMillis.toInt.toUInt

  // uv_fs_event_t / uv_fs_poll_t allocation: a null calloc result is OOM, surfaced by a throw.
  // scalafix:off DisableSyntax
  private def allocHandle(ordinal: Int): Ptr[Byte] =
    val handle = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(ordinal))
    if handle == null then throw new OutOfMemoryError("emile: fs watch handle allocation failed")
    else handle
  // scalafix:on DisableSyntax

  private def changeBit(change: FSChange): Int = change match
    case FSChange.Renamed => LibUV.UV_RENAME
    case FSChange.Changed => LibUV.UV_CHANGE

  private def decodeChanges(events: Int): Set[FSChange] =
    FSChange.values.iterator.filter(change => (events & changeBit(change)) != 0).toSet

  // libuv passes a NULL filename when the backend supplies no entry name (e.g. a watched file itself).
  // scalafix:off DisableSyntax
  private def decodeFilename(filename: CString): Option[String] =
    if filename == null then None else Some(fromCString(filename))
  // scalafix:on DisableSyntax

  // Offers a change to both subscriber views from the loop thread: events losslessly; changes coalesced
  // to one outstanding pulse by the changePending latch, with a terminal error always delivered through.
  private def offer(state: FSState, event: Either[EmileError.IO, FSEvent]): Unit =
    state.eventsQueue.unsafeOffer(event)
    event match
      // A watch error is terminal: deliver it through, so it ends the changes stream too.
      case Left(error) => state.changesQueue.unsafeOffer(Left(error))
      // Coalesce: offer a pulse only when none is outstanding; the consumer clears the latch on take.
      case Right(_) =>
        if !state.changePending then
          state.changePending = true
          state.changesQueue.unsafeOffer(Right(()))

  // uv_fs_event_cb: build the change event and offer it; a negative status is a typed watch error.
  private val fsEventCb: LibUV.FSEventCB = (handle: Ptr[Byte], filename: CString, events: CInt, status: CInt) =>
    val state = CallbackBridge.load[FSState](handle)
    if status < 0 then offer(state, Left(IOMapping.fromCode(status)))
    else offer(state, Right(FSEvent(decodeChanges(events), decodeFilename(filename))))

  // uv_fs_poll_cb: fires only on a stat transition - status < 0 means the path became inaccessible
  // (Renamed), status == 0 means it appeared or its stat changed (Changed). No entry name is available.
  private val fsPollCb: LibUV.FSPollCB = (handle: Ptr[Byte], status: CInt, _: Ptr[Byte], _: Ptr[Byte]) =>
    val state = CallbackBridge.load[FSState](handle)
    val change = if status < 0 then FSChange.Renamed else FSChange.Changed
    offer(state, Right(FSEvent(Set(change), None)))

end FS
