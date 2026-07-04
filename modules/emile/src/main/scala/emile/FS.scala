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

import scala.concurrent.duration.FiniteDuration
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.CInt
import scala.scalanative.unsafe.CString
import scala.scalanative.unsafe.CUnsignedInt
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.fromCString
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
  def watch(path: java.nio.file.Path): EmResource[EmileError.IO, FS] =
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
  def poll(path: java.nio.file.Path, interval: FiniteDuration): EmResource[EmileError.IO, FS] =
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
