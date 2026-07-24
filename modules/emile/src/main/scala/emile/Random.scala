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

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.Routing

/** System-CSPRNG random bytes (`uv_random`), filled on libuv's worker threadpool.
  *
  * The output is cryptographically strong - `getrandom(2)`, falling back to `/dev/urandom`. This is
  * the runtime's general-purpose randomness; where an application also runs a dedicated
  * cryptographic library, draw key material, tokens, and protocol identifiers from that library's
  * own generator, so a single audited generator owns the security-critical randomness rather than
  * two coexisting on one classpath. The concern is ownership, not the quality of these bytes.
  *
  * A fill can, in principle, stall while the system gathers entropy at early boot; it never returns
  * a short fill - the buffer is filled completely or the effect fails.
  */
object Random:

  /** Fills `slice` with random bytes; an empty slice is a no-op. The region is borrowed until the
    * effect completes.
    */
  def fill(slice: Slice): EmIO[EmileError.IO, Unit] =
    if slice.isEmpty then EffIO.succeed(())
    else
      EffIO.attempt(
        LibUVPollingSystem.currentPoller.flatMap(poller => randomFill(poller, slice.unsafePtr, slice.length, slice)),
        EmileError.IO.Unexpected(_)
      )

  /** A fresh array of `n` random bytes; `n` must be non-negative (a negative count is
    * [[EmileError.IO.InvalidArgument]]), and `0` yields an empty array.
    */
  def bytes(n: Int): EmIO[EmileError.IO, Array[Byte]] =
    if n < 0 then EffIO.fail(EmileError.IO.InvalidArgument(s"random byte count must be non-negative, was $n"))
    else if n == 0 then EffIO.succeed(new Array[Byte](0))
    else
      val array = new Array[Byte](n)
      EffIO.attempt(
        LibUVPollingSystem.currentPoller.flatMap(poller => randomFill(poller, array.atUnsafe(0), n, array)).as(array),
        EmileError.IO.Unexpected(_)
      )

  // FFI: request alloc null-check and free, buffer keep-alive holder, callback recovery.
  // scalafix:off DisableSyntax

  // uv_random with flags 0; async, so it runs on the threadpool. `keepAlive` holds the fill buffer
  // reachable until the callback fires. A queued fill cancels to UV_ECANCELED like any threadpool op.
  private def randomFill(poller: LibUVPoller, buf: Ptr[Byte], len: Int, keepAlive: AnyRef): IO[Unit] =
    IO.async[Unit]: cb =>
      Routing.onOwner(poller):
        val req = allocRandomRequest()
        val rc = LibUV.uv_random(poller.loop, req, buf, len.toCSize, 0.toUInt, randomCb)
        if rc < 0 then
          stdlib.free(req)
          cb(Left(IOMapping.fromCode(rc)))
          None
        else
          CallbackBridge.storeReq(poller, req, new RandomState(poller, cb, keepAlive))
          Some(Routing.onOwner(poller)(LibUV.uv_cancel(req): Unit))

  // The anchored completion for a uv_random request: the callback and the fill buffer, held reachable
  // while the request is outstanding.
  final private class RandomState(
    val poller: LibUVPoller,
    val cb: Either[Throwable, Unit] => Unit,
    @scala.annotation.unused val keepAlive: AnyRef
  )

  // uv_random is not an fs request, so the uv_random_t is freed directly (no uv_fs_req_cleanup).
  private val randomCb: LibUV.RandomCB = (req: Ptr[Byte], status: CInt, _: Ptr[Byte], _: CSize) =>
    val state = CallbackBridge.loadReq[RandomState](req)
    CallbackBridge.releaseReq(state.poller, req)
    stdlib.free(req)
    if status < 0 then state.cb(Left(IOMapping.fromCode(status))) else state.cb(Right(()))

  private def allocRandomRequest(): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(LibUV.UV_RANDOM))
    if req == null then throw new OutOfMemoryError("emile: uv_random_t allocation failed")
    else req

  // scalafix:on DisableSyntax

end Random
