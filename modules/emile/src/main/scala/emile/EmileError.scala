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
  * [[EmileError.Io Io]], [[EmileError.Dns Dns]], [[EmileError.Runtime Runtime]]). Each domain
  * offers named cases for the common failures, a `System(code)` catch-all for any other libuv code,
  * and an idempotent `Unexpected(cause)` wrapping a raw `Throwable` - an already-typed cause is
  * returned unchanged.
  */
object EmileError:

  /** Failures from `Tcp.bind`. */
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

  /** Failures from `Tcp.connect` to an `IpAddress`; through [[HostConnect]] the hostname overload
    * unifies these with [[Dns]].
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

  /** Common parent of [[Connect]] and [[Dns]] - the error type of `Tcp.connect(host, port)`. */
  sealed trait HostConnect extends EmileError

  /** Failures from I/O on a live handle - socket reads, writes, and half-closes, file reads, and
    * fd-readiness.
    */
  sealed trait Io extends EmileError

  object Io:
    case object EndOfStream extends EmileError("End of stream", None) with Io
    case object ConnectionReset extends EmileError("Connection reset", None) with Io
    case object BrokenPipe extends EmileError("Broken pipe", None) with Io
    case object AlreadyClosed extends EmileError("Resource already closed", None) with Io
    case object ConflictingTransfer
        extends EmileError("A sendFile cannot overlap a concurrent write, sendFile, or half-close on the same socket", None)
        with Io

    final case class System(code: ErrorCode) extends EmileError("", None) with Io:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Io:
      override def getMessage: String = s"Unexpected I/O failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Io = cause match
        case e: Io => e
        case t => new Unexpected(t)
  end Io

  /** Failures from `Dns.resolve` / `Dns.reverse`. */
  sealed trait Dns extends HostConnect

  object Dns:
    final case class UnknownHost(host: String) extends EmileError(s"Unknown host: $host", None) with Dns

    final case class TemporaryFailure(host: String) extends EmileError(s"Temporary name-resolution failure: $host", None) with Dns

    final case class System(code: ErrorCode) extends EmileError("", None) with Dns:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Dns:
      override def getMessage: String = s"Unexpected DNS failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Dns = cause match
        case e: Dns => e
        case t => new Unexpected(t)

  /** Programmer errors and runtime invariants - surfaced through cats-effect's `Throwable` channel
    * by `absolve`, not through the per-operation typed channels.
    */
  sealed trait Runtime extends EmileError

  object Runtime:
    case object MissingLibuvPollingSystem
        extends EmileError(
          "LibuvPollingSystem is not installed in this IORuntime. Use EmileIOApp or Emile.runtime.",
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

/** Maps a libuv error code to a typed [[EmileError.Io]], falling through to
  * [[EmileError.Io.System]] for codes with no dedicated case.
  */
private[emile] object IoMapping:
  def fromCode(code: Int): EmileError.Io = code match
    case ErrorCode.UV_EOF => EmileError.Io.EndOfStream
    case ErrorCode.UV_ECONNRESET => EmileError.Io.ConnectionReset
    case ErrorCode.UV_EPIPE => EmileError.Io.BrokenPipe
    case other => EmileError.Io.System(ErrorCode(other))

/** Maps a libuv resolver code, with the host being resolved, to a typed [[EmileError.Dns]]. The
  * `getaddrinfo` name-resolution codes become `UnknownHost` (which carries the host); all others
  * become `System`.
  */
private[emile] object DnsMapping:
  def fromCode(code: Int, host: String): EmileError.Dns = code match
    case ErrorCode.UV_EAI_NONAME | ErrorCode.UV_EAI_NODATA => EmileError.Dns.UnknownHost(host)
    case ErrorCode.UV_EAI_AGAIN => EmileError.Dns.TemporaryFailure(host)
    case other => EmileError.Dns.System(ErrorCode(other))
