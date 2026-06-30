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

import scala.util.control.NoStackTrace

import cats.data.NonEmptyList

/** Sealed root of every typed error emile surfaces. The whole hierarchy - a sub-trait per failure
  * domain, each with its named cases - lives in [[EmileError$ EmileError]].
  *
  * It extends `Exception` so the value `absolve` carries into cats-effect's `Throwable` channel is
  * the error itself, still recoverable and matchable. `getMessage` is overridden per case for lazy
  * derivation; the base constructor receives an empty-string placeholder.
  */
@scala.annotation.internal.sharable
sealed abstract class EmileError(message: String, cause: Option[Throwable])
    extends Exception(message, cause.orNull[Throwable | Null])
    with NoStackTrace derives CanEqual

/** The [[EmileError]] hierarchy - one sealed sub-trait per failure domain ([[EmileError.Bind
  * Bind]], [[EmileError.Connect Connect]], [[EmileError.HostConnect HostConnect]],
  * [[EmileError.IO IO]], [[EmileError.DNS DNS]], [[EmileError.Runtime Runtime]]). Each domain
  * offers named cases for the common failures, a `System(code)` catch-all for any other libuv code,
  * and an idempotent `Unexpected(cause)` wrapping a raw `Throwable` - an already-typed cause is
  * returned unchanged.
  */
object EmileError:

  /** Failures from `TCP.bind`. */
  sealed trait Bind extends EmileError

  object Bind:
    case object AddressInUse extends EmileError("Address in use", None) with Bind
    case object AddressNotAvailable extends EmileError("Address not available", None) with Bind
    case object PermissionDenied extends EmileError("Permission denied", None) with Bind

    final case class InvalidAddress(detail: String) extends EmileError(s"Invalid address: $detail", None) with Bind

    final case class System(code: ErrorCode) extends EmileError("", None) with Bind:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Bind:
      override def getMessage: String = s"Unexpected bind failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Bind = cause match
        case e: Bind => e
        case t => new Unexpected(t)
  end Bind

  /** Failures from `TCP.connect` to an `IpAddress`; through [[HostConnect]] the hostname overload
    * unifies these with [[DNS]].
    */
  sealed trait Connect extends HostConnect

  object Connect:
    case object ConnectionRefused extends EmileError("Connection refused", None) with Connect
    case object NetworkUnreachable extends EmileError("Network unreachable", None) with Connect
    case object HostUnreachable extends EmileError("Host unreachable", None) with Connect
    case object TimedOut extends EmileError("Connection timed out", None) with Connect

    /** Every address a hostname resolved to failed to connect; `failures` carries the per-address
      * [[Connect]] errors in resolver order.
      */
    final case class AllAddressesFailed(failures: NonEmptyList[Connect]) extends EmileError("", None) with Connect:
      override def getMessage: String =
        s"All ${failures.size} resolved addresses failed to connect (last: ${failures.last.getMessage})"

    final case class System(code: ErrorCode) extends EmileError("", None) with Connect:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Connect:
      override def getMessage: String = s"Unexpected connect failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Connect = cause match
        case e: Connect => e
        case t => new Unexpected(t)
  end Connect

  /** Common parent of [[Connect]] and [[DNS]] - the error type of `TCP.connect(host, port)`. */
  sealed trait HostConnect extends EmileError

  /** Failures from I/O on a live handle - socket reads, writes, and half-closes, file reads, and
    * fd-readiness.
    */
  sealed trait IO extends EmileError

  object IO:
    case object EndOfStream extends EmileError("End of stream", None) with IO
    case object ConnectionReset extends EmileError("Connection reset", None) with IO
    case object BrokenPipe extends EmileError("Broken pipe", None) with IO
    case object AlreadyClosed extends EmileError("Resource already closed", None) with IO
    case object ConflictingTransfer
        extends EmileError("A sendFile cannot overlap a concurrent write, sendFile, or half-close on the same socket", None)
        with IO

    final case class System(code: ErrorCode) extends EmileError("", None) with IO:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with IO:
      override def getMessage: String = s"Unexpected I/O failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): IO = cause match
        case e: IO => e
        case t => new Unexpected(t)
  end IO

  /** Failures from `DNS.resolve` / `DNS.reverse`. */
  sealed trait DNS extends HostConnect

  object DNS:
    final case class UnknownHost(host: String) extends EmileError(s"Unknown host: $host", None) with DNS

    final case class TemporaryFailure(host: String) extends EmileError(s"Temporary name-resolution failure: $host", None) with DNS

    final case class System(code: ErrorCode) extends EmileError("", None) with DNS:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with DNS:
      override def getMessage: String = s"Unexpected DNS failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): DNS = cause match
        case e: DNS => e
        case t => new Unexpected(t)

  /** Programmer errors and runtime invariants - surfaced through cats-effect's `Throwable` channel
    * by `absolve`, not through the per-operation typed channels.
    */
  sealed trait Runtime extends EmileError

  object Runtime:
    case object MissingLibUVPollingSystem
        extends EmileError(
          "LibUVPollingSystem is not installed in this IORuntime. Use EmileIOApp or Emile.runtime.",
          None
        )
        with Runtime

    final case class System(code: ErrorCode) extends EmileError("", None) with Runtime:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Runtime:
      override def getMessage: String = s"Unexpected runtime failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Runtime = cause match
        case e: Runtime => e
        case t => new Unexpected(t)
  end Runtime
end EmileError

/** Maps a libuv error code to a typed [[EmileError.Bind]], falling through to
  * [[EmileError.Bind.System]] for codes with no dedicated case.
  */
private[emile] object BindMapping:
  def fromCode(code: Int): EmileError.Bind = code match
    case ErrorCode.UV_EADDRINUSE => EmileError.Bind.AddressInUse
    case ErrorCode.UV_EADDRNOTAVAIL => EmileError.Bind.AddressNotAvailable
    case ErrorCode.UV_EACCES => EmileError.Bind.PermissionDenied
    case other => EmileError.Bind.System(ErrorCode(other))

/** Maps a libuv error code to a typed [[EmileError.Connect]], falling through to
  * [[EmileError.Connect.System]] for codes with no dedicated case.
  */
private[emile] object ConnectMapping:
  def fromCode(code: Int): EmileError.Connect = code match
    case ErrorCode.UV_ECONNREFUSED => EmileError.Connect.ConnectionRefused
    case ErrorCode.UV_ENETUNREACH => EmileError.Connect.NetworkUnreachable
    case ErrorCode.UV_EHOSTUNREACH => EmileError.Connect.HostUnreachable
    case ErrorCode.UV_ETIMEDOUT => EmileError.Connect.TimedOut
    case other => EmileError.Connect.System(ErrorCode(other))

/** Maps a libuv error code to a typed [[EmileError.IO]], falling through to
  * [[EmileError.IO.System]] for codes with no dedicated case.
  */
private[emile] object IOMapping:
  def fromCode(code: Int): EmileError.IO = code match
    case ErrorCode.UV_EOF => EmileError.IO.EndOfStream
    case ErrorCode.UV_ECONNRESET => EmileError.IO.ConnectionReset
    case ErrorCode.UV_EPIPE => EmileError.IO.BrokenPipe
    case other => EmileError.IO.System(ErrorCode(other))

/** Maps a libuv resolver code, with the host being resolved, to a typed [[EmileError.DNS]]. The
  * `getaddrinfo` name-resolution codes become `UnknownHost` (which carries the host); all others
  * become `System`.
  */
private[emile] object DNSMapping:
  def fromCode(code: Int, host: String): EmileError.DNS = code match
    case ErrorCode.UV_EAI_NONAME | ErrorCode.UV_EAI_NODATA => EmileError.DNS.UnknownHost(host)
    case ErrorCode.UV_EAI_AGAIN => EmileError.DNS.TemporaryFailure(host)
    case other => EmileError.DNS.System(ErrorCode(other))
