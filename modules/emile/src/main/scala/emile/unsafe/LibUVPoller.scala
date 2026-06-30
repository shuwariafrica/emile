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
package emile.unsafe

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.concurrent.TrieMap
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.signal
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr
import scala.scalanative.runtime.toRawPtr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import cats.effect.unsafe.PollResult

import emile.EmileError
import emile.ErrorCode
import emile.LoopConfig

/** The per-worker libuv event loop: a `uv_loop_t`, the cross-thread interrupt and task-submission
  * `uv_async_t`s, and the `uv_timer_t` that bounds a timed [[poll]]. One instance backs each
  * cats-effect worker; the libuv C callbacks live in [[LibUVPoller$ LibUVPoller]].
  */
// libuv-loop FFI: poller-state vars, C-loop drain/pump while-loops, poll's early returns, null
// handle/thread sentinels, init-failure throws, callback-recovery asInstanceOf - no
// allocation-free idiomatic alternative.
// scalafix:off DisableSyntax
final private[emile] class LibUVPoller(val config: LoopConfig):

  /** The `uv_loop_t` - `calloc`'d at `uv_loop_size()`, `uv_loop_init`'d, then tuned by [[config]]
    * through `uv_loop_configure`. Every step is checked and throws on failure, so a
    * half-initialised poller is never handed back.
    */
  val loop: Ptr[Byte] =
    val l = stdlib.calloc(1.toCSize, LibUV.uv_loop_size())
    if l == null then throw new OutOfMemoryError("emile: libuv loop allocation failed")
    checked(LibUV.uv_loop_init(l))
    if config.blockProfilerSignal then checked(LibUV.uv_loop_configure(l, LibUV.UV_LOOP_BLOCK_SIGNAL, signal.SIGPROF))
    if config.useIoUringSqpoll then checked(LibUV.uv_loop_configure(l, LibUV.UV_LOOP_USE_IO_URING_SQPOLL))
    l

  @volatile private var ownerThread: Thread | Null = null

  private val interruptAsync: Ptr[Byte] = checkedAlloc(LibUV.UV_ASYNC)
  private val taskAsync: Ptr[Byte] = checkedAlloc(LibUV.UV_ASYNC)
  private val timeoutTimer: Ptr[Byte] = checkedAlloc(LibUV.UV_TIMER)
  private[unsafe] val taskQueue: ConcurrentLinkedQueue[Runnable] = new ConcurrentLinkedQueue()

  /** GC-reachability anchor for holders stored via [[CallbackBridge]] in libuv handle / request
    * `data` slots, keyed by the slot's pointer address. `castObjectToRawPtr` is a pure
    * reinterpretation, and Scala Native's GC does not scan C-allocated memory; the anchor keeps
    * each stored holder reachable for as long as the C-side raw pointer must remain valid.
    */
  private[unsafe] val anchors: TrieMap[Long, AnyRef] = new TrieMap[Long, AnyRef]()

  // Outstanding libuv requests keyed by address, maintained by CallbackBridge.storeReq / releaseReq, so
  // close() can uv_cancel any in-flight threadpool request and avoid a busy-spin / hang at shutdown.
  private[unsafe] val outstandingReqs: TrieMap[Long, Ptr[Byte]] = new TrieMap[Long, Ptr[Byte]]()

  @volatile private[unsafe] var interrupted: Boolean = false
  @volatile private[unsafe] var closed: Boolean = false

  // uv_*_init never writes handle->data, so storing `this` after init assumes no struct layout.
  checked(LibUV.uv_async_init(loop, interruptAsync, LibUVPoller.interruptCb))
  checked(LibUV.uv_async_init(loop, taskAsync, LibUVPoller.taskDrainCb))
  checked(LibUV.uv_timer_init(loop, timeoutTimer))
  LibUV.uv_handle_set_data(interruptAsync, asRawData(this))
  LibUV.uv_handle_set_data(taskAsync, asRawData(this))
  LibUV.uv_handle_set_data(timeoutTimer, asRawData(this))

  private def asRawData(self: AnyRef): Ptr[Byte] =
    fromRawPtr[Byte](Intrinsics.castObjectToRawPtr(self))

  private def checked(rc: Int): Unit =
    if rc != 0 then throw EmileError.Runtime.System(ErrorCode(rc))

  private def checkedAlloc(handleType: Int): Ptr[Byte] =
    val p = stdlib.calloc(1.toCSize, LibUV.uv_handle_size(handleType))
    if p == null then throw new OutOfMemoryError("emile: libuv handle allocation failed")
    else p

  /** Whether the calling thread is the loop's owner - the thread that first ran [[poll]]. */
  def isOwnerThread: Boolean =
    val t = ownerThread
    (t ne null) && (Thread.currentThread() eq t)

  /** Enqueue a runnable for the loop thread and wake the loop; `false` if the poller has closed. */
  def submit(r: Runnable): Boolean =
    if closed then false
    else
      taskQueue.offer(r): Unit
      LibUV.uv_async_send(taskAsync): Unit
      true

  /** Remove a still-queued runnable - the cross-thread cancellation path. */
  def remove(r: Runnable): Boolean = taskQueue.remove(r)

  /** Run one libuv loop iteration bounded by `nanos` (`0` non-blocking, `-1` block until an event),
    * then return so the worker can resume fibres and re-poll.
    */
  def poll(nanos: Long): PollResult =
    if ownerThread eq null then ownerThread = Thread.currentThread()
    if closed then return PollResult.Interrupted
    if interrupted then
      interrupted = false
      return PollResult.Interrupted

    nanos match
      case 0L => LibUV.uv_run(loop, LibUV.UV_RUN_NOWAIT): Unit
      // No timer armed: uv__backend_timeout yields uv__next_timeout's empty-heap -1, uv__io_poll
      // blocks, and a uv_async_send from interrupt() / submit() ends the iteration.
      case -1L => LibUV.uv_run(loop, LibUV.UV_RUN_ONCE): Unit
      case n if n > 0L =>
        val ms = n / 1_000_000L
        // libuv timer granularity is whole milliseconds; a sub-millisecond wait cannot be honoured
        // by blocking, so poll non-blocking rather than over-sleep past `nanos`.
        if ms <= 0L then LibUV.uv_run(loop, LibUV.UV_RUN_NOWAIT): Unit
        else
          LibUV.uv_timer_start(timeoutTimer, LibUVPoller.timeoutWakeCb, ms.toULong, 0.toULong): Unit
          LibUV.uv_run(loop, LibUV.UV_RUN_ONCE): Unit
          LibUV.uv_timer_stop(timeoutTimer): Unit
      case _ => LibUV.uv_run(loop, LibUV.UV_RUN_NOWAIT): Unit
    end match

    // One uv_run iteration drains every ready event, so a non-interrupted result is always
    // Complete; Incomplete (re-poll for more events) never applies to a libuv backend.
    if interrupted then
      interrupted = false
      PollResult.Interrupted
    else PollResult.Complete
  end poll

  /** Whether the loop still has active handles or requests and should be polled again. */
  def needsPoll: Boolean = !closed && LibUV.uv_loop_alive(loop) != 0

  /** Mark the poller interrupted and wake the loop - the cross-thread interrupt path. */
  def interrupt(): Unit =
    if !closed then
      interrupted = true
      LibUV.uv_async_send(interruptAsync): Unit

  /** Close the poller at runtime shutdown: drop queued tasks, `uv_close` every handle still on the
    * loop, pump the loop until the close callbacks fire, then close and free the loop.
    */
  def close(): Unit =
    closed = true
    // Drop queued tasks unrun: close() runs only at shutdown, after every fibre is cancelled, so a
    // remaining task's awaiting IO.async is already gone.
    while taskQueue.poll() != null do ()
    // Cancel outstanding threadpool requests directly: close() drives the loop itself and cannot route
    // through onOwner, and a cancelled request's callback fires promptly so the blocking drain below
    // terminates. uv_cancel on a non-cancelable (stream) request is a harmless no-op; those are aborted
    // by the uv_walk handle close instead.
    outstandingReqs.values.foreach(req => LibUV.uv_cancel(req): Unit)
    LibUV.uv_walk(loop, LibUVPoller.closeWalkCb, loop)
    // Blocking ONCE drain, not a NOWAIT 100% spin: each iteration parks until an event. Shutdown
    // latency is bounded by the slowest still-uncancelable in-flight request.
    while LibUV.uv_loop_alive(loop) != 0 do LibUV.uv_run(loop, LibUV.UV_RUN_ONCE): Unit
    LibUV.uv_loop_close(loop): Unit
    stdlib.free(loop)
  end close

end LibUVPoller

/** The libuv C callbacks for [[LibUVPoller]]. Each is an object-scope `val` capturing no instance,
  * as a `CFuncPtr` must; the owning poller is recovered from the handle's `data` slot.
  */
object LibUVPoller:

  private inline def pollerOf(handle: Ptr[Byte]): LibUVPoller =
    Intrinsics.castRawPtrToObject(toRawPtr(LibUV.uv_handle_get_data(handle))).asInstanceOf[LibUVPoller]

  // Drains the cross-thread task queue. The submitted runnables capture every throwable and complete
  // their own IO.async callback, so this blanket catch is only a backstop: a throw must never cross
  // the C ABI.
  private val taskDrainCb: LibUV.AsyncCB = (handle: Ptr[Byte]) =>
    val poller = pollerOf(handle)
    var r = poller.taskQueue.poll()
    while r != null do
      try r.run()
      catch case _: Throwable => ()
      r = poller.taskQueue.poll()

  // Empty by design: the uv_async_send wakes the loop; the interrupted flag is read by the next poll.
  private val interruptCb: LibUV.AsyncCB = (_: Ptr[Byte]) => ()

  // Empty by design: firing wakes the loop when a timed poll's deadline expires.
  private val timeoutWakeCb: LibUV.TimerCB = (_: Ptr[Byte]) => ()

  // uv_walk callback for close: closes every handle that is not already closing.
  private val closeWalkCb: LibUV.WalkCB = (handle: Ptr[Byte], _: Ptr[Byte]) =>
    if LibUV.uv_is_closing(handle) == 0 then LibUV.uv_close(handle, LibUVPoller.freeHandleCb)

  // uv_close callback: frees a handle's C memory once libuv has finished with it.
  private val freeHandleCb: LibUV.CloseCB = (handle: Ptr[Byte]) => stdlib.free(handle)

end LibUVPoller
// scalafix:on DisableSyntax
