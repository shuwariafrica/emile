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
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.effect.Resource
import fs2.Chunk
import fs2.Stream

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing

final private class OpenFileState(val file: Int, val poller: LibUVPoller)

/** A file open through libuv, backed by a `uv_file` descriptor. Acquired through
  * [[OpenFile$ OpenFile]]; the descriptor is closed when the resource releases. Whether it may be
  * read, written, or both follows the [[OpenMode]] it was opened with.
  */
opaque type OpenFile = OpenFileState

/** Resource, read and write operations, metadata, and equality for [[OpenFile]]. Every file
  * operation runs on libuv's process-wide worker threadpool - shared with [[DNS$ DNS]] resolution
  * across every loop, four threads by default - so set `UV_THREADPOOL_SIZE` in the environment
  * before startup when concurrent file I/O saturates it.
  */
object OpenFile:

  /** Opens `path` read-only; the file must already exist. The descriptor is closed when the
    * resource releases, and while open the same file may serve repeated reads.
    */
  def open(path: Path): EmResource[EmileError.IO, OpenFile] =
    open(path, LibUV.UV_FS_O_RDONLY, 0)

  /** Opens `path` per `mode`, creating it with permission `0644` where the mode creates. */
  def open(path: Path, mode: OpenMode): EmResource[EmileError.IO, OpenFile] =
    open(path, OpenMode.flags(mode), DefaultCreatePermissions)

  /** Opens `path` per `mode`, creating it with `permissions` (an octal mode, effective only when
    * the mode creates the file).
    */
  def open(path: Path, mode: OpenMode, permissions: Int): EmResource[EmileError.IO, OpenFile] =
    open(path, OpenMode.flags(mode), permissions)

  private def open(path: Path, flags: Int, permissions: Int): EmResource[EmileError.IO, OpenFile] =
    Resource.makeFull[EffIO.Of[EmileError.IO], OpenFile] { poll =>
      EffIO
        .liftF(LibUVPollingSystem.currentPoller)
        .flatMap: poller =>
          // poll wraps only the open wait; the fd -> OpenFileState adoption is uncancelable, so an fd
          // delivered as cancellation lands is still adopted - and closed by the resource's release - and
          // never orphaned.
          poll(EffIO.attempt(openFile(poller, path, flags, permissions), EmileError.IO.Unexpected(_)))
            .map(new OpenFileState(_, poller))
    }(release)

  given CanEqual[OpenFile, OpenFile] = CanEqual.derived

  /** Read-chunk size for [[reads]]. */
  inline val DefaultReadSize = 65536

  // The permission a two-argument create-mode open uses (0644 octal): owner read/write, others read.
  private inline val DefaultCreatePermissions = 0x1a4

  extension (file: OpenFile)
    /** The file's size in bytes. */
    def size: EmIO[EmileError.IO, Long] =
      EffIO.attempt(statSize(file), EmileError.IO.Unexpected(_))

    /** Read up to `maxBytes` from the current position, advancing it; `None` at end of file. */
    def read(maxBytes: Int): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
      readOnce(file, maxBytes)

    /** The file's bytes from the current position as a stream - the backpressure-correct large-body
      * source for `file.reads.through(socket.writes)`.
      */
    def reads: EmStream[EmileError.IO, Byte] =
      Stream.resource(readsResource(file)).flatMap(state => Stream.repeatEval(readsRead(state))).unNoneTerminate.unchunks

    /** Writes every byte of `slice` at the current position, advancing it. The region is borrowed
      * until the effect completes, so do not mutate it while the write is in flight; a reusable
      * write scratch is an owned array framed as a [[boilerplate.Slice Slice]] per fill, its
      * completion sequenced before the next fill. A short native write is completed by resubmitting
      * the remainder, so a genuine error - never a partial write - surfaces.
      */
    def write(slice: Slice): EmIO[EmileError.IO, Unit] =
      EffIO.attempt(writeAll(file, slice, -1L), EmileError.IO.Unexpected(_))

    /** Writes every byte of `slice` at `offset` without moving the current position. Under an
      * append mode the kernel ignores `offset` and the bytes still land at end of file. Borrowing
      * and short-write handling are as for the current-position [[write]].
      */
    def write(slice: Slice, offset: Long): EmIO[EmileError.IO, Unit] =
      EffIO.attempt(writeAll(file, slice, offset), EmileError.IO.Unexpected(_))

    /** A sink writing every byte the source emits at the current position, chunk by chunk - the
      * append-a-record shape for a file logger over `stream.through(file.writes)`.
      */
    def writes: EmPipe[EmileError.IO, Byte, Nothing] =
      _.chunks.foreach(chunk => writeChunkAt(file, chunk))

    /** Truncates or extends the file to `size` bytes (`ftruncate`); an extension reads back as
      * zeros.
      */
    def truncate(size: Long): EmIO[EmileError.IO, Unit] =
      fsUnit(file)(req => LibUV.uv_fs_ftruncate(file.poller.loop, req, file.file, size, fsDeliveryCb))

    /** Flushes buffered data and metadata to the storage device (`fsync`). */
    def sync: EmIO[EmileError.IO, Unit] =
      fsUnit(file)(req => LibUV.uv_fs_fsync(file.poller.loop, req, file.file, fsDeliveryCb))

    /** Flushes buffered data - but not size-unchanged metadata - to the storage device
      * (`fdatasync`).
      */
    def datasync: EmIO[EmileError.IO, Unit] =
      fsUnit(file)(req => LibUV.uv_fs_fdatasync(file.poller.loop, req, file.file, fsDeliveryCb))

    /** The file's full [[FileStatus]] (`fstat`). */
    def stat: EmIO[EmileError.IO, FileStatus] =
      EffIO.attempt(
        fsRequest(file, noBuffer)(req => LibUV.uv_fs_fstat(file.poller.loop, req, file.file, fsDeliveryCb))(statusResult),
        EmileError.IO.Unexpected(_)
      )

    /** Sets the file's permission bits to `permissions` (an octal mode) via `fchmod`. */
    def chmod(permissions: Int): EmIO[EmileError.IO, Unit] =
      fsUnit(file)(req => LibUV.uv_fs_fchmod(file.poller.loop, req, file.file, permissions, fsDeliveryCb))

    /** Sets the file's owner and group via `fchown`; either id may be `-1` to leave it unchanged. */
    def chown(uid: Int, gid: Int): EmIO[EmileError.IO, Unit] =
      fsUnit(file)(req => LibUV.uv_fs_fchown(file.poller.loop, req, file.file, uid.toUInt, gid.toUInt, fsDeliveryCb))

    /** Sets the file's access and modification times via `futime`; each [[SetTime]] is set
      * explicitly, to now, or left unchanged independently.
      */
    def setTimes(atime: SetTime, mtime: SetTime): EmIO[EmileError.IO, Unit] =
      fsUnit(file)(req =>
        LibUV.uv_fs_futime(file.poller.loop, req, file.file, SetTime.toDouble(atime), SetTime.toDouble(mtime), fsDeliveryCb)
      )
  end extension

  // Adopts an already-open descriptor (from mkstemp) into an OpenFile on `poller`; closed through the
  // shared release. The file supports the operations its underlying open flags allow.
  private[emile] def fromDescriptor(poller: LibUVPoller, fd: Int): OpenFile = new OpenFileState(fd, poller)

  private[emile] def close(file: OpenFile): IO[Unit] = closeFile(file)

  /** The underlying `uv_file` descriptor - for `uv_fs_sendfile`. */
  private[emile] def descriptor(file: OpenFile): Int = file.file

  /** The owning loop's poller - for `uv_fs_sendfile`'s loop routing. */
  private[emile] def owner(file: OpenFile): LibUVPoller = file.poller

  private def release(file: OpenFile): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(closeFile(file))

  // Carries the open's callback and a cancelled flag - set by the cancellation finaliser, read by
  // openDeliver, both on the loop thread, so it needs no synchronisation.
  final private class OpenRequest(val cb: Either[Throwable, Int] => Unit):
    var cancelled: Boolean = false // scalafix:ok DisableSyntax.var

  // uv_fs_open; the request result delivered by the callback is the uv_file descriptor.
  private def openFile(poller: LibUVPoller, path: Path, flags: Int, permissions: Int): IO[Int] =
    IO.async[Int]: cb =>
      Routing.onOwner(poller):
        val request = new OpenRequest(cb)
        val req = allocRequest()
        val rc = startOpen(poller, req, path, flags, permissions)
        if rc < 0 then
          failRequest(req, cb, rc)
          None
        else
          CallbackBridge.storeReq(poller, req, openDeliver(poller, request))
          // uv_cancel only stops a still-queued open. If one a worker has begun completes anyway, its
          // callback fires after this cb is dead, so the flag tells openDeliver to close the delivered
          // fd rather than orphan it. A queued open cancels to UV_ECANCELED, an error with no fd.
          Some(Routing.onOwner(poller) { request.cancelled = true; LibUV.uv_cancel(req): Unit })

  private def statSize(file: OpenFile): IO[Long] =
    IO.async[Long]: cb =>
      Routing.onOwner(file.poller):
        val req = allocRequest()
        val rc = LibUV.uv_fs_fstat(file.poller.loop, req, file.file, fsCb)
        if rc < 0 then
          failRequest(req, cb, rc)
          None
        else
          CallbackBridge.storeReq(file.poller, req, statDeliver(file.poller, cb))
          Some(Routing.onOwner(file.poller)(LibUV.uv_cancel(req): Unit))

  private def readOnce(file: OpenFile, maxBytes: Int): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
    EffIO.attempt(readFile(file, maxBytes), EmileError.IO.Unexpected(_))

  // The per-read buffer is freed in readDeliver on every path: uv_cancel only stops a still-queued
  // read, so one a threadpool worker has already begun still runs to completion and must reclaim it.
  private def readFile(file: OpenFile, maxBytes: Int): IO[Option[Chunk[Byte]]] =
    IO.async[Option[Chunk[Byte]]]: cb =>
      Routing.onOwner(file.poller):
        val req = allocRequest()
        val buf = allocReadBuffer(maxBytes)
        val bufs = stackalloc[LibUV.Buf]()
        bufs._1 = buf
        bufs._2 = maxBytes.toCSize
        val rc = LibUV.uv_fs_read(file.poller.loop, req, file.file, bufs, 1.toUInt, -1L, fsCb)
        if rc < 0 then
          stdlib.free(buf)
          failRequest(req, cb, rc)
          None
        else
          CallbackBridge.storeReq(file.poller, req, readDeliver(file.poller, cb, buf))
          Some(Routing.onOwner(file.poller)(LibUV.uv_cancel(req): Unit))

  // uv_fs_close; the descriptor is treated as released whatever the close result.
  private def closeFile(file: OpenFile): IO[Unit] =
    IO.async[Unit]: cb =>
      Routing.onOwner(file.poller):
        val req = allocRequest()
        val rc = LibUV.uv_fs_close(file.poller.loop, req, file.file, fsCb)
        if rc < 0 then
          cleanupRequest(req)
          cb(Right(()))
        else CallbackBridge.storeReq(file.poller, req, closeDeliver(file.poller, cb))
        None

  private def startOpen(poller: LibUVPoller, req: Ptr[Byte], path: Path, flags: Int, permissions: Int): Int =
    Zone(LibUV.uv_fs_open(poller.loop, req, toCString(path.toString), flags, permissions, fsCb))

  // Synchronous uv_fs_* failure: it occurs before storeReq, so there is no anchor to release - just
  // clean up the request and fail the callback.
  private def failRequest[A](req: Ptr[Byte], cb: Either[Throwable, A] => Unit, rc: Int): Unit =
    cleanupRequest(req)
    cb(Left(IOMapping.fromCode(rc)))

  private def openDeliver(poller: LibUVPoller, request: OpenRequest): Ptr[Byte] => Unit =
    req =>
      val result = LibUV.uv_fs_get_result(req).toInt
      CallbackBridge.releaseReq(poller, req)
      cleanupRequest(req)
      if result < 0 then request.cb(Left(IOMapping.fromCode(result)))
      // The open completed after a cancel (uv_cancel could not stop it); the cb is dead, so close the
      // delivered fd rather than leak it.
      else if request.cancelled then closeOrphan(poller, result)
      else request.cb(Right(result))

  // Fire-and-forget close of an fd delivered after its acquire was cancelled - no fiber awaits it.
  private def closeOrphan(poller: LibUVPoller, fd: Int): Unit =
    val req = allocRequest()
    if LibUV.uv_fs_close(poller.loop, req, fd, orphanCloseCb) < 0 then cleanupRequest(req)

  private val orphanCloseCb: LibUV.FSCB = (req: Ptr[Byte]) => cleanupRequest(req)

  private def statDeliver(poller: LibUVPoller, cb: Either[Throwable, Long] => Unit): Ptr[Byte] => Unit =
    req =>
      val result = LibUV.uv_fs_get_result(req).toInt
      val outcome: Either[EmileError.IO, Long] =
        if result < 0 then Left(IOMapping.fromCode(result))
        else Right(LibUV.uv_fs_get_statbuf(req)._8.toLong)
      CallbackBridge.releaseReq(poller, req)
      cleanupRequest(req)
      cb(outcome)

  private def readDeliver(
    poller: LibUVPoller,
    cb: Either[Throwable, Option[Chunk[Byte]]] => Unit,
    buf: Ptr[Byte]
  ): Ptr[Byte] => Unit =
    req =>
      val result = LibUV.uv_fs_get_result(req).toInt
      val outcome: Either[EmileError.IO, Option[Chunk[Byte]]] =
        if result < 0 then Left(IOMapping.fromCode(result))
        else if result == 0 then Right(None)
        else Right(Some(Chunk.fromBytePtr(buf, result)))
      stdlib.free(buf)
      CallbackBridge.releaseReq(poller, req)
      cleanupRequest(req)
      cb(outcome)

  // reads reuses one buffer across the stream's sequential chunks instead of a malloc per chunk (the
  // one-shot read keeps its own buffer - independent reads cannot share one). The read arm, its deliver,
  // and the release all run on the loop thread, so reading / releasePending are owner-confined; the
  // deliver runs after the threadpool worker has finished writing, so freeing there - or at release when
  // no read is in flight - never frees a buffer a worker is still writing. A naive per-stream buffer,
  // freed unconditionally at release, would race that off-loop write; this deferred free closes it.
  final private class ReadsState(val file: OpenFileState, val buffer: ResizableBuffer):
    var reading: Boolean = false // scalafix:ok DisableSyntax.var
    var releasePending: Boolean = false // scalafix:ok DisableSyntax.var

  private def readsResource(file: OpenFileState): EmResource[EmileError.IO, ReadsState] =
    Resource.make[EffIO.Of[EmileError.IO], ReadsState](EffIO.liftF(IO(new ReadsState(file, ResizableBuffer(DefaultReadSize)))))(
      readsRelease
    )

  private def readsRelease(state: ReadsState): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(
      Routing.onOwner(state.file.poller):
        if state.reading then state.releasePending = true
        else state.buffer.free()
    )

  private def readsRead(state: ReadsState): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
    EffIO.attempt(
      IO.async[Option[Chunk[Byte]]]: cb =>
        Routing.onOwner(state.file.poller):
          val poller = state.file.poller
          val req = allocRequest()
          val buf = state.buffer.ensure(DefaultReadSize)
          val bufs = stackalloc[LibUV.Buf]()
          bufs._1 = buf
          bufs._2 = DefaultReadSize.toCSize
          val rc = LibUV.uv_fs_read(poller.loop, req, state.file.file, bufs, 1.toUInt, -1L, fsCb)
          if rc < 0 then
            failRequest(req, cb, rc)
            None
          else
            state.reading = true
            CallbackBridge.storeReq(poller, req, readsDeliver(state, cb, buf))
            Some(Routing.onOwner(poller)(LibUV.uv_cancel(req): Unit))
      ,
      EmileError.IO.Unexpected(_)
    )

  private def readsDeliver(
    state: ReadsState,
    cb: Either[Throwable, Option[Chunk[Byte]]] => Unit,
    buf: Ptr[Byte]
  ): Ptr[Byte] => Unit =
    req =>
      val result = LibUV.uv_fs_get_result(req).toInt
      // Copy into the owned Chunk before any free; the reused buffer is overwritten by the next read.
      val outcome: Either[EmileError.IO, Option[Chunk[Byte]]] =
        if result < 0 then Left(IOMapping.fromCode(result))
        else if result == 0 then Right(None)
        else Right(Some(Chunk.fromBytePtr(buf, result)))
      state.reading = false
      // The release deferred the free to here - the worker has finished, so the buffer is now idle.
      if state.releasePending then state.buffer.free()
      CallbackBridge.releaseReq(state.file.poller, req)
      cleanupRequest(req)
      cb(outcome)

  private def closeDeliver(poller: LibUVPoller, cb: Either[Throwable, Unit] => Unit): Ptr[Byte] => Unit =
    req =>
      CallbackBridge.releaseReq(poller, req)
      cleanupRequest(req)
      cb(Right(()))

  // Loop-to-completion write: uv_fs_write can report a short count when a late error is swallowed
  // (never a benign short write - it retries EINTR and short kernel writes internally), so the
  // remainder is resubmitted until complete; the swallowed error then surfaces on the resubmit, whose
  // fresh attempt returns it. A positioned write advances its offset by the bytes written; a
  // current-position write keeps offset -1.
  private def writeAll(file: OpenFile, slice: Slice, offset: Long): IO[Unit] =
    if slice.isEmpty then IO.unit
    else
      writeOnce(file, slice, offset).flatMap: written =>
        if written >= slice.length then IO.unit
        else writeAll(file, slice.drop(written), if offset < 0L then -1L else offset + written)

  // One uv_fs_write; the slice is the keep-alive, holding its backing region reachable for the whole
  // in-flight write. The stack bufs array is copied by uv_fs_write during the call, so it need not
  // outlive it.
  private def writeOnce(file: OpenFile, slice: Slice, offset: Long): IO[Int] =
    fsRequest(file, slice) { req =>
      val bufs = stackalloc[LibUV.Buf]()
      bufs._1 = slice.unsafePtr
      bufs._2 = slice.length.toCSize
      LibUV.uv_fs_write(file.poller.loop, req, file.file, bufs, 1.toUInt, offset, fsDeliveryCb)
    }(intResult)

  // Each chunk's backing array framed as a Slice for the shared write-to-completion path.
  private def writeChunkAt(file: OpenFile, chunk: Chunk[Byte]): EmIO[EmileError.IO, Unit] =
    if chunk.isEmpty then EffIO.succeed(())
    else
      val arr = chunk.toArraySlice
      EffIO.attempt(writeAll(file, Slice.of(arr.values, arr.offset, arr.length), -1L), EmileError.IO.Unexpected(_))

  // A result-0-or-typed-error fs op on `file`'s loop.
  private def fsUnit(file: OpenFile)(submit: Ptr[Byte] => Int): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(fsRequest(file, noBuffer)(submit)(unitResult), EmileError.IO.Unexpected(_))

  // Submit an fs op on `file`'s loop and deliver its typed result. `submit(req)` issues the uv_fs_*
  // call (returning its rc); `interpret(req)` reads the outcome from the completed request, before the
  // cleanup that frees it. `keepAlive` is held reachable (via the anchored delivery) until the
  // callback fires - the write buffer especially. Cancellation cancels a still-queued op; its callback
  // then delivers UV_ECANCELED and reclaims the request like any other completion.
  private def fsRequest[A](file: OpenFile, keepAlive: AnyRef)(submit: Ptr[Byte] => Int)(
    interpret: Ptr[Byte] => Either[EmileError.IO, A]): IO[A] =
    IO.async[A]: cb =>
      Routing.onOwner(file.poller):
        val req = allocRequest()
        val rc = submit(req)
        if rc < 0 then
          failRequest(req, cb, rc)
          None
        else
          val run: Ptr[Byte] => Unit = r =>
            val outcome = interpret(r)
            CallbackBridge.releaseReq(file.poller, r)
            cleanupRequest(r)
            cb(outcome)
          CallbackBridge.storeReq(file.poller, req, new FsDelivery(run, keepAlive))
          Some(Routing.onOwner(file.poller)(LibUV.uv_cancel(req): Unit))

  // The anchored completion for an fsRequest: its delivery closure, plus the buffer the op borrows,
  // both kept reachable while the request is outstanding.
  final private class FsDelivery(val run: Ptr[Byte] => Unit, @scala.annotation.unused val keepAlive: AnyRef)

  private val fsDeliveryCb: LibUV.FSCB = (req: Ptr[Byte]) => CallbackBridge.loadReq[FsDelivery](req).run(req)

  // Keep-alive placeholder for ops that borrow no caller buffer.
  private val noBuffer: AnyRef = new Object

  private def unitResult(req: Ptr[Byte]): Either[EmileError.IO, Unit] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(())

  private def intResult(req: Ptr[Byte]): Either[EmileError.IO, Int] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(result)

  private def statusResult(req: Ptr[Byte]): Either[EmileError.IO, FileStatus] =
    val result = LibUV.uv_fs_get_result(req).toInt
    if result < 0 then Left(IOMapping.fromCode(result)) else Right(FileStatus.fromStat(LibUV.uv_fs_get_statbuf(req)))

  private def cleanupRequest(req: Ptr[Byte]): Unit =
    LibUV.uv_fs_req_cleanup(req)
    stdlib.free(req)

  // uv_fs_t allocation: a null calloc result is OOM, surfaced by a throw.
  // scalafix:off DisableSyntax
  private def allocRequest(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_FS))
    if req == null then throw new OutOfMemoryError("emile: uv_fs_t allocation failed")
    else req

  private def allocReadBuffer(maxBytes: Int): Ptr[Byte] =
    val buf = stdlib.malloc(maxBytes.toCSize)
    if buf == null then throw new OutOfMemoryError("emile: read buffer allocation failed")
    else buf
  // scalafix:on DisableSyntax

  // uv_fs_cb: run the per-request delivery closure stored in the request's data slot.
  private val fsCb: LibUV.FSCB = (req: Ptr[Byte]) => CallbackBridge.loadReq[Ptr[Byte] => Unit](req).apply(req)

end OpenFile
