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
import cats.effect.std.unsafe.BoundedQueue
import fs2.Chunk
import fs2.Stream

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.LiveHandle
import emile.unsafe.ResizableBuffer
import emile.unsafe.Routing

// The mutable per-handle state a uv_stream_t byte stream drives: the live handle, the per-direction
// read buffer, and the read/write guard flags. A uv_tty_t is a uv_stream_t, so a Tty and a Socket both
// carry one and share StreamCore; a socket adds its addresses in a subclass.
private[emile] class StreamState(val live: LiveHandle, val readBuffer: ResizableBuffer):
  // Mutual exclusion of a raw-fd sendFile with queued writes and a half-close. sendFile bypasses libuv's
  // write queue (and the FIN ordering uv_shutdown derives from it), so an overlap interleaves on the wire
  // or, against a half-close, sends FIN past the in-flight sendFile and truncates. Owner-confined - every
  // write / sendFile / shutdown submit and completion runs on the loop thread - so these need no barrier.
  // outputShutdown is terminal: once endOfOutput's uv_shutdown is submitted the write side is closed.
  var pendingWrites: Int = 0 // scalafix:ok DisableSyntax.var
  var sendFileActive: Boolean = false // scalafix:ok DisableSyntax.var
  var outputShutdown: Boolean = false // scalafix:ok DisableSyntax.var
  // Single-reader guard. Every read mode drives the one per-handle readBuffer and the handle's single
  // read-callback slot, so a second concurrent reader overwrites the first's in-flight read - silent
  // corruption. Held for a read's whole span, including the f that consumes the read's borrowed slice, so
  // a concurrent reader fails fast instead. Owner-confined like the flags above.
  var reading: Boolean = false // scalafix:ok DisableSyntax.var
  // The in-flight read's terminal action, set when a read arms and cleared when it stops - non-null
  // exactly while a receiver holds the read-callback slot. A socket's closeReset fires it before it stops
  // the read and repurposes the slot, so a concurrent reader is completed rather than left hanging.
  var readTerminate: (EmileError.IO => Unit) | Null = null // scalafix:ok DisableSyntax.var, DisableSyntax.null
end StreamState

/** The shared `uv_stream_t` byte-stream engine over [[StreamState]]: the read modes ([[reads]], the
  * one-shot and borrowed reads, [[consumeAll]]), the write modes (copying, gathering, and zero-copy
  * over a [[boilerplate.Slice Slice]]), the single-reader guard, and generic reclamation. A
  * [[Socket$ Socket]] and a [[Tty$ Tty]] are typed facades over it, so the byte surface is defined
  * and tested once and every kind rides the same machinery.
  */
private[emile] object StreamCore:

  // Buffer size for the persistent read modes - one recv cluster without excess GC pressure.
  inline val DefaultReadSize = 65536

  // Back-pressure queue depth behind reads; the loop-thread staging slot absorbs the overflow chunk.
  inline val ReadsQueueCapacity = 4

  // Back-pressured: the watcher arms once, and readsReceiver / readsResume pause and resume libuv as
  // the consumer keeps up or falls behind.
  def reads(state: StreamState): EmStream[EmileError.IO, Byte] =
    Stream.resource(readsResource(state)).flatMap(rs => Stream.repeatEval(readsPull(rs))).unNoneTerminate.unchunks

  def writes(state: StreamState): EmPipe[EmileError.IO, Byte, Nothing] =
    _.chunks.foreach(chunk => writeChunk(state, chunk))

  // FFI: handle/req allocation null-checks, read-receiver var/null sentinels, stackalloc fd cell for
  // uv_fileno, chunk-reachability holder, C-bridge asInstanceOf recoveries.
  // scalafix:off DisableSyntax

  // Stored in the handle's data slot; the alloc/read trampolines invoke its two closures, so each read
  // mode supplies its own alloc/deliver behaviour without a dedicated trampoline. deliver receives the
  // live handle libuv called back on, so a read callback never reaches for the stored (freeable) one.
  // terminate fails this mode's own continuation with a given error, for a socket's closeReset to end an
  // in-flight read that it is about to stop.
  final private case class ReadReceiver(
    alloc: (CSize, Ptr[LibUV.Buf]) => Unit,
    deliver: (Ptr[Byte], CSSize, Ptr[LibUV.Buf]) => Unit,
    terminate: EmileError.IO => Unit
  )

  // Store a read receiver in the handle's data slot and record its terminal action, so a socket's
  // closeReset can end the read. Paired with clearRead, which removes both.
  private def armReceiver(state: StreamState, handle: Ptr[Byte], receiver: ReadReceiver): Unit =
    CallbackBridge.store(poller(state), handle, receiver)
    state.readTerminate = receiver.terminate

  private val allocCb: LibUV.AllocCB = (handle: Ptr[Byte], suggested: CSize, bufOut: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[ReadReceiver](handle).alloc(suggested, bufOut)

  private val readCb: LibUV.ReadCB = (handle: Ptr[Byte], nread: CSSize, buf: Ptr[LibUV.Buf]) =>
    CallbackBridge.load[ReadReceiver](handle).deliver(handle, nread, buf)

  // Record a completed read delivery in the loop's metrics: data or a clean end-of-stream is a success,
  // any other negative status a failure. Called from each receiver on the loop thread.
  private def recordRead(state: StreamState, nreadInt: Int): Unit =
    poller(state).metrics.readSettled(nreadInt > 0 || nreadInt == ErrorCode.UV_EOF)

  // The single-reader guard wrapping each public read entry. The claim is taken on the owner thread and
  // released on every outcome; bracket takes it uncancelably, keeps the read itself cancelable, and skips
  // the release when the claim was not taken. The persistent reads stream uses readingResource for the
  // same effect across the Resource scope. read(maxBytes) / readN keep the EmileError.IO channel; the
  // borrowed-slice read / consume run withReadingApp, whose claim widens onto the union channel (its
  // AlreadyClosed / ConflictingOperation are the EmileError.IO arm).
  private def withReading[A](state: StreamState)(body: => EmIO[EmileError.IO, A]): EmIO[EmileError.IO, A] =
    acquireReading(state).bracket(_ => body)(_ => releaseReading(state))

  private def withReadingApp[E <: Throwable, A](state: StreamState)(body: => EmIO[EmileError.IO | E, A]): EmIO[EmileError.IO | E, A] =
    val acquired: EmIO[EmileError.IO | E, Unit] = acquireReading(state)
    acquired.bracket(_ => body)(_ => releaseReading(state))

  private def readingResource(state: StreamState): EmResource[EmileError.IO, Unit] =
    Resource.make[EffIO.Of[EmileError.IO], Unit](acquireReading(state))(_ => EffIO.liftF(releaseReading(state)))

  private def acquireReading(state: StreamState): EmIO[EmileError.IO, Unit] =
    EffIO.lift(Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, closedIo)(_ => claimReading(state))))

  // Owner-thread check-and-set of the single-reader flag.
  private def claimReading(state: StreamState): Either[EmileError.IO, Unit] =
    if state.reading then Left(EmileError.IO.ConflictingOperation)
    else
      state.reading = true
      Right(())

  private def releaseReading(state: StreamState): IO[Unit] =
    Routing.onOwner(poller(state))(state.reading = false)

  def readOnce(state: StreamState, maxBytes: Int): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
    // A non-positive size would hand libuv a zero/oversized buffer length (recv past the buffer); reject it.
    if maxBytes < 1 then EffIO.fail(EmileError.IO.InvalidArgument(s"read size must be at least 1, was $maxBytes"))
    else withReading(state)(readOnceArm(state, maxBytes))

  private def readOnceArm(state: StreamState, maxBytes: Int): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
    EffIO.attempt(
      IO.async[Option[Chunk[Byte]]] { cb =>
        Routing.onOwner(poller(state)):
          LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
            armReceiver(state, handle, oneShotReceiver(state, cb, maxBytes))
            val rc = LibUV.uv_read_start(handle, allocCb, readCb)
            if rc < 0 then
              clearRead(state, handle)
              cb(Left(IOMapping.fromCode(rc)))
              None
            else stopReadFinaliser(state)
      },
      EmileError.IO.Unexpected(_)
    )

  private def oneShotReceiver(
    state: StreamState,
    cb: Either[Throwable, Option[Chunk[Byte]]] => Unit,
    maxBytes: Int
  ): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = state.readBuffer.ensure(maxBytes)
        bufOut._1 = ptr
        bufOut._2 = maxBytes.toCSize
      ,
      deliver = (handle, nread, buf) =>
        // nread == 0 is the libuv EAGAIN sentinel; wait for the next read_cb without delivering.
        if nread != 0 then
          stopRead(state, handle)
          val nreadInt = nread.toInt
          recordRead(state, nreadInt)
          if nreadInt > 0 then cb(Right(Some(Chunk.fromBytePtr(buf._1, nreadInt))))
          else if nreadInt == ErrorCode.UV_EOF then cb(Right(None))
          else cb(Left(IOMapping.fromCode(nreadInt))),
      terminate = err => cb(Left(err))
    )

  def readNBytes(state: StreamState, numBytes: Int): EmIO[EmileError.IO, Chunk[Byte]] =
    withReading(state)(readNBytesLoop(state, numBytes))

  // Accumulates across bare-arm reads under one held claim, so a concurrent reader cannot interleave
  // between the chunks and steal bytes from the stream.
  private def readNBytesLoop(state: StreamState, numBytes: Int): EmIO[EmileError.IO, Chunk[Byte]] =
    def go(acc: Chunk[Byte]): EmIO[EmileError.IO, Chunk[Byte]] =
      if acc.size >= numBytes then EffIO.succeed(acc)
      else
        readOnceArm(state, numBytes - acc.size).flatMap:
          case Some(chunk) => go(acc ++ chunk)
          case None => EffIO.fail(EmileError.IO.EndOfStream)
    go(Chunk.empty[Byte])

  def readPtrOnce[E <: Throwable, A](
    state: StreamState,
    f: Slice => EmIO[E, A]
  ): EmIO[EmileError.IO | E, Option[A]] =
    withReadingApp(state)(readPtrOnceArm(state, f))

  // The read failure widens onto the EmileError.IO arm of the union and f's own failure onto the E arm.
  // The delivered (ptr, len) is framed as a borrowed Slice at the hand-off; f must not retain it.
  private def readPtrOnceArm[E <: Throwable, A](
    state: StreamState,
    f: Slice => EmIO[E, A]
  ): EmIO[EmileError.IO | E, Option[A]] =
    val delivered: EmIO[EmileError.IO | E, Option[(Ptr[Byte], Int)]] = readPtrDeliver(state)
    delivered.flatMap:
      case Some((ptr, len)) => f(Slice.of(ptr, len)).map(Some(_))
      case None => EffIO.succeed(None)

  private def readPtrDeliver(state: StreamState): EmIO[EmileError.IO, Option[(Ptr[Byte], Int)]] =
    EffIO.attempt(
      IO.async[Option[(Ptr[Byte], Int)]] { cb =>
        Routing.onOwner(poller(state)):
          LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
            armReceiver(state, handle, readPtrReceiver(state, cb))
            val rc = LibUV.uv_read_start(handle, allocCb, readCb)
            if rc < 0 then
              clearRead(state, handle)
              cb(Left(IOMapping.fromCode(rc)))
              None
            else stopReadFinaliser(state)
      },
      EmileError.IO.Unexpected(_)
    )

  private def readPtrReceiver(
    state: StreamState,
    cb: Either[Throwable, Option[(Ptr[Byte], Int)]] => Unit
  ): ReadReceiver =
    ReadReceiver(
      alloc = (suggested, bufOut) =>
        val capacity = if suggested.toInt > 0 then suggested.toInt else DefaultReadSize
        val ptr = state.readBuffer.ensure(capacity)
        bufOut._1 = ptr
        bufOut._2 = capacity.toCSize
      ,
      deliver = (handle, nread, buf) =>
        if nread != 0 then
          stopRead(state, handle)
          val nreadInt = nread.toInt
          recordRead(state, nreadInt)
          if nreadInt > 0 then cb(Right(Some((buf._1, nreadInt))))
          else if nreadInt == ErrorCode.UV_EOF then cb(Right(None))
          else cb(Left(IOMapping.fromCode(nreadInt))),
      terminate = err => cb(Left(err))
    )

  final private class ReadsState(
    val state: StreamState,
    val queue: BoundedQueue[IO, Either[EmileError.IO, Option[Chunk[Byte]]]]
  ):
    // Touched only on the handle's loop thread.
    var pending: Either[EmileError.IO, Option[Chunk[Byte]]] | Null = null
    var paused: Boolean = false
    var terminated: Boolean = false

  private def readsResource(state: StreamState): EmResource[EmileError.IO, ReadsState] =
    readingResource(state).flatMap(_ => Resource.make[EffIO.Of[EmileError.IO], ReadsState](readsAcquire(state))(readsRelease))

  private def readsAcquire(state: StreamState): EmIO[EmileError.IO, ReadsState] =
    EffIO.lift(
      for
        queue <- BoundedQueue[IO, Either[EmileError.IO, Option[Chunk[Byte]]]](ReadsQueueCapacity)
        reads = new ReadsState(state, queue)
        result <- Routing.onOwner(poller(state))(readsInstall(reads))
      yield result
    )

  private def readsInstall(reads: ReadsState): Either[EmileError.IO, ReadsState] =
    LiveHandle.tryUse(reads.state.live, closedIo.map(_ => reads)): handle =>
      armReceiver(reads.state, handle, readsReceiver(reads))
      val rc = LibUV.uv_read_start(handle, allocCb, readCb)
      if rc < 0 then
        clearRead(reads.state, handle)
        Left(IOMapping.fromCode(rc))
      else Right(reads)

  private def readsRelease(reads: ReadsState): EmIO[EmileError.IO, Unit] =
    EffIO.liftF(
      Routing.onOwner(poller(reads.state))(
        LiveHandle.tryUse(reads.state.live, ())(handle => stopRead(reads.state, handle))
      )
    )

  private def readsReceiver(reads: ReadsState): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = reads.state.readBuffer.ensure(DefaultReadSize)
        bufOut._1 = ptr
        bufOut._2 = DefaultReadSize.toCSize
      ,
      deliver = (handle, nread, buf) =>
        if nread != 0 then
          val nreadInt = nread.toInt
          recordRead(reads.state, nreadInt)
          val item: Either[EmileError.IO, Option[Chunk[Byte]]] =
            if nreadInt > 0 then Right(Some(Chunk.fromBytePtr(buf._1, nreadInt)))
            else if nreadInt == ErrorCode.UV_EOF then Right(None)
            else Left(IOMapping.fromCode(nreadInt))
          val terminal = item match
            case Right(Some(_)) => false
            case _ => true
          if terminal then reads.terminated = true
          if reads.queue.unsafeTryOffer(item) then
            if terminal then LibUV.uv_read_stop(handle): Unit
          else
            // Stage in the one-slot pending area and pause; the next reader pull restores it.
            reads.pending = item
            reads.paused = true
            LibUV.uv_read_stop(handle): Unit
      ,
      terminate = err => readsTerminateWith(reads, err)
    )

  private def readsPull(reads: ReadsState): EmIO[EmileError.IO, Option[Chunk[Byte]]] =
    EffIO.lift(
      reads.queue.take.flatMap(item => Routing.onOwner(poller(reads.state))(readsResume(reads)).as(item))
    )

  // Re-arms the paused read on a pull. Guarded: if the handle released between pulls, terminate the
  // stream with a typed error rather than re-arming a freed handle.
  private def readsResume(reads: ReadsState): Unit =
    LiveHandle.tryUse(reads.state.live, readsTerminateWith(reads, EmileError.IO.AlreadyClosed)): handle =>
      val pending = reads.pending
      if pending ne null then
        reads.pending = null
        if !reads.queue.unsafeTryOffer(pending) then
          // Queue still full - re-stage and stay paused. The next pull retries.
          reads.pending = pending
        else if reads.paused && !reads.terminated then readsRearm(reads, handle)
      else if reads.paused && !reads.terminated then readsRearm(reads, handle)

  private def readsRearm(reads: ReadsState, handle: Ptr[Byte]): Unit =
    reads.paused = false
    val rc = LibUV.uv_read_start(handle, allocCb, readCb)
    if rc < 0 then
      reads.terminated = true
      reads.queue.unsafeTryOffer(Left(IOMapping.fromCode(rc))): Unit

  // Ends the reads stream with a typed error, once. Shared by a pull that finds the handle released
  // (AlreadyClosed) and by a socket's closeReset terminating an in-flight stream.
  private def readsTerminateWith(reads: ReadsState, err: EmileError.IO): Unit =
    if !reads.terminated then
      reads.terminated = true
      reads.queue.unsafeTryOffer(Left(err)): Unit

  def consumeAll[E <: Throwable](state: StreamState, onChunk: Slice => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
    withReadingApp(state)(consumeAllArm(state, onChunk))

  // asyncAttempt folds a registration-time throwable only, so it never sees - and so never mis-wraps - the
  // typed outcome the callback delivers on the same channel. Unexpected is idempotent, so Routing's own
  // AlreadyClosed passes through and only a true defect is wrapped.
  private def consumeAllArm[E <: Throwable](state: StreamState, onChunk: Slice => Either[E, Unit]): EmIO[EmileError.IO | E, Unit] =
    EffIO.asyncAttempt[EmileError.IO | E, Unit](EmileError.IO.Unexpected(_)) { cb =>
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse(state.live, closedConsume(cb)): handle =>
          armReceiver(state, handle, consumeReceiver(state, cb, onChunk))
          val rc = LibUV.uv_read_start(handle, allocCb, readCb)
          if rc < 0 then
            clearRead(state, handle)
            cb(Left(IOMapping.fromCode(rc)))
            None
          else stopReadFinaliser(state)
    }

  private def closedConsume[E <: Throwable](cb: Either[EmileError.IO | E, Unit] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.IO.AlreadyClosed))
    Option.empty[IO[Unit]]

  private def consumeReceiver[E <: Throwable](
    state: StreamState,
    cb: Either[EmileError.IO | E, Unit] => Unit,
    onChunk: Slice => Either[E, Unit]
  ): ReadReceiver =
    ReadReceiver(
      alloc = (_, bufOut) =>
        val ptr = state.readBuffer.ensure(DefaultReadSize)
        bufOut._1 = ptr
        bufOut._2 = DefaultReadSize.toCSize
      ,
      deliver = (handle, nread, buf) =>
        if nread != 0 then
          val nreadInt = nread.toInt
          recordRead(state, nreadInt)
          if nreadInt > 0 then
            val outcome: Either[EmileError.IO | E, Unit] =
              try onChunk(Slice.of(buf._1, nreadInt))
              catch case t: Throwable => Left(EmileError.IO.Unexpected(t))
            outcome match
              case Right(()) => () // keep the watcher armed for the next chunk
              case left =>
                stopRead(state, handle)
                cb(left)
          else if nreadInt == ErrorCode.UV_EOF then
            stopRead(state, handle)
            cb(Right(()))
          else
            stopRead(state, handle)
            cb(Left(IOMapping.fromCode(nreadInt)))
      ,
      terminate = err => cb(Left(err))
    )

  // Keeps the written region reachable across the in-flight uv_write, and carries the poller the writeCb
  // trampoline needs to release the request's anchor.
  final private class WriteState(
    val state: StreamState,
    val poller: LibUVPoller,
    val cb: Either[Throwable, Unit] => Unit,
    @scala.annotation.unused val keepAlive: AnyRef
  )

  def writeChunk(state: StreamState, chunk: Chunk[Byte]): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(state)):
          if chunk.isEmpty then
            cb(Right(()))
            None
          else
            LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
              val slice = chunk.toArraySlice
              val ptr = slice.values.atUnsafe(slice.offset)
              submitWrite(state, handle, ptr, slice.length, cb, slice.values)
              None
      },
      EmileError.IO.Unexpected(_)
    )

  // The keep-alive is the Slice itself: an array-backed slice roots its backing array transitively,
  // and a pointer-backed slice holds no reference, matching Slice's own caller-keeps-alive contract.
  def writeSlice(state: StreamState, slice: Slice): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(state)):
          if slice.isEmpty then
            cb(Right(()))
            None
          else
            LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
              submitWrite(state, handle, slice.unsafePtr, slice.length, cb, slice)
              None
      },
      EmileError.IO.Unexpected(_)
    )

  def writeSlices(state: StreamState, slices: Seq[Slice]): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(state)):
          val buffers = slices.iterator.filterNot(_.isEmpty).toVector
          if buffers.isEmpty then
            cb(Right(()))
            None
          else
            LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
              submitWriteV(state, handle, buffers, cb)
              None
      },
      EmileError.IO.Unexpected(_)
    )

  // A Chunk's backing array framed as a Slice, for the shared vectored-write path. The Slice roots the
  // array through its anchor, keeping it reachable across the in-flight write.
  private def chunkToSlice(chunk: Chunk[Byte]): Slice =
    val arr = chunk.toArraySlice
    Slice.of(arr.values, arr.offset, arr.length)

  private def submitWrite(
    state: StreamState,
    handle: Ptr[Byte],
    base: Ptr[Byte],
    length: Int,
    cb: Either[Throwable, Unit] => Unit,
    keepAlive: AnyRef
  ): Unit =
    if state.sendFileActive || state.outputShutdown then cb(Left(EmileError.IO.ConflictingOperation))
    else
      val req = allocWriteReq()
      val bufs = stackalloc[LibUV.Buf]()
      bufs._1 = base
      bufs._2 = length.toCSize
      CallbackBridge.storeReq(poller(state), req, new WriteState(state, poller(state), cb, keepAlive))
      val rc = LibUV.uv_write(req, handle, bufs, 1.toUInt, writeCb)
      if rc < 0 then
        CallbackBridge.releaseReq(poller(state), req)
        stdlib.free(req)
        cb(Left(IOMapping.fromCode(rc)))
      else
        state.pendingWrites += 1
        poller(state).metrics.writeStarted()
  end submitWrite

  private val writeCb: LibUV.WriteCB = (req: Ptr[Byte], status: CInt) =>
    val write = CallbackBridge.loadReq[WriteState](req)
    CallbackBridge.releaseReq(write.poller, req)
    stdlib.free(req)
    write.state.pendingWrites -= 1
    write.poller.metrics.writeSettled(status)
    // A local close / closeReset flushes queued writes with UV_ECANCELED; report it as AlreadyClosed
    // (the local-teardown domain the reader also gets), not a System fault the peer did not cause.
    if status == ErrorCode.UV_ECANCELED then write.cb(Left(EmileError.IO.AlreadyClosed))
    else if status < 0 then write.cb(Left(IOMapping.fromCode(status)))
    else write.cb(Right(()))

  def writeChunks(state: StreamState, chunks: Seq[Chunk[Byte]]): EmIO[EmileError.IO, Unit] =
    EffIO.attempt(
      IO.async[Unit] { cb =>
        Routing.onOwner(poller(state)):
          val buffers = chunks.iterator.filter(_.nonEmpty).map(chunkToSlice).toVector
          if buffers.isEmpty then
            cb(Right(()))
            None
          else
            LiveHandle.tryUse(state.live, closedAsync(cb)): handle =>
              submitWriteV(state, handle, buffers, cb)
              None
      },
      EmileError.IO.Unexpected(_)
    )

  // At or below this many buffers the uv_buf_t array is stack-allocated (2 KB of stack), so the HTTP/2
  // hot path of many small frames touches no heap; a larger gather falls back to a heap array.
  private inline val VectoredStackThreshold = 128

  private def submitWriteV(
    state: StreamState,
    handle: Ptr[Byte],
    slices: Vector[Slice],
    cb: Either[Throwable, Unit] => Unit
  ): Unit =
    if state.sendFileActive || state.outputShutdown then cb(Left(EmileError.IO.ConflictingOperation))
    else
      val nbufs = slices.size
      // uv_write copies the bufs array during the call, so it need not outlive uv_write; the stack
      // array stays valid across the synchronous writeVectored call, and the heap one is freed right
      // after. The regions must outlive the write and stay reachable through WriteState - the Vector of
      // Slices roots each array-backed anchor; a pointer-backed slice is the caller's to keep alive.
      if nbufs <= VectoredStackThreshold then writeVectored(state, handle, slices, stackalloc[LibUV.Buf](nbufs), cb)
      else
        val bufs = allocBufs(nbufs)
        writeVectored(state, handle, slices, bufs, cb)
        stdlib.free(bufs)
  end submitWriteV

  private def writeVectored(
    state: StreamState,
    handle: Ptr[Byte],
    slices: Vector[Slice],
    bufs: Ptr[LibUV.Buf],
    cb: Either[Throwable, Unit] => Unit
  ): Unit =
    val nbufs = slices.size
    val req = allocWriteReq()
    var i = 0
    while i < nbufs do
      val slice = slices(i)
      val buf = bufs + i
      buf._1 = slice.unsafePtr
      buf._2 = slice.length.toCSize
      i += 1
    CallbackBridge.storeReq(poller(state), req, new WriteState(state, poller(state), cb, slices))
    val rc = LibUV.uv_write(req, handle, bufs, nbufs.toUInt, writeCb)
    if rc < 0 then
      CallbackBridge.releaseReq(poller(state), req)
      stdlib.free(req)
      cb(Left(IOMapping.fromCode(rc)))
    else
      state.pendingWrites += 1
      poller(state).metrics.writeStarted()
  end writeVectored

  private def allocBufs(nbufs: Int): Ptr[LibUV.Buf] =
    val bufs = stdlib.calloc(nbufs.toCSize, sizeof[LibUV.Buf])
    if bufs == null then throw new OutOfMemoryError("emile: uv_buf_t array allocation failed")
    else bufs.asInstanceOf[Ptr[LibUV.Buf]]

  def tryWriteSlice(state: StreamState, slice: Slice): EmIO[EmileError.IO, Int] =
    EffIO.lift(
      Routing.onOwner(poller(state)):
        LiveHandle.tryUse[Either[EmileError.IO, Int]](state.live, Left(EmileError.IO.AlreadyClosed)): handle =>
          if state.sendFileActive || state.outputShutdown then Left(EmileError.IO.ConflictingOperation)
          else if slice.isEmpty then Right(0)
          else
            val bufs = stackalloc[LibUV.Buf]()
            bufs._1 = slice.unsafePtr
            bufs._2 = slice.length.toCSize
            val rc = LibUV.uv_try_write(handle, bufs, 1.toUInt)
            // uv_try_write returns the bytes accepted now; UV_EAGAIN means the buffer is full, so zero.
            if rc >= 0 then Right(rc)
            else if rc == ErrorCode.UV_EAGAIN then Right(0)
            else Left(IOMapping.fromCode(rc))
    )

  // The shared reclamation for Socket and Tty: stop any in-flight read before reclaiming the handle,
  // so no callback fires against it between the close request and the free; then free the read buffer.
  def release(state: StreamState): IO[Unit] =
    Routing
      .onOwner(poller(state))(LiveHandle.tryUse(state.live, ())(handle => stopRead(state, handle)))
      .flatMap(_ => LiveHandle.closeOnOwner(state.live))
      .flatMap(_ => IO(state.readBuffer.free()))

  // The owning loop's poller - always valid (it is stored alongside the handle, not freed with it).
  private def poller(state: StreamState): LibUVPoller = LiveHandle.poller(state.live)

  // The closed-branch result for a handle access that returns a typed Either.
  private val closedIo: Either[EmileError.IO, Unit] = Left(EmileError.IO.AlreadyClosed)

  // The closed-branch result for a handle access registered through IO.async: fail the callback with
  // AlreadyClosed and register no finaliser. By-name in tryUse, so it fires only when closed.
  private def closedAsync[A](cb: Either[Throwable, A] => Unit): Option[IO[Unit]] =
    cb(Left(EmileError.IO.AlreadyClosed))
    Option.empty[IO[Unit]]

  // The cancellation finaliser shared by every one-shot read: stop the read on the owner, guarded so a
  // cancel that races release never touches a freed handle.
  private def stopReadFinaliser(state: StreamState): Option[IO[Unit]] =
    Some(Routing.onOwner(poller(state))(LiveHandle.tryUse(state.live, ())(handle => stopRead(state, handle))))

  // Clear the read-callback slot and its terminal-action record, without stopping the watch - for an
  // arm that failed before the read started.
  private def clearRead(state: StreamState, handle: Ptr[Byte]): Unit =
    CallbackBridge.clear(poller(state), handle)
    state.readTerminate = null

  private def stopRead(state: StreamState, handle: Ptr[Byte]): Unit =
    LibUV.uv_read_stop(handle): Unit
    clearRead(state, handle)

  private def allocWriteReq(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_WRITE))
    if req == null then throw new OutOfMemoryError("emile: uv_write_t allocation failed")
    else req

  // scalafix:on DisableSyntax

end StreamCore
