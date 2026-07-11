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
  *
  * Every case is a type as well as a value, so an [[EmIO]] channel narrows to the failures an
  * operation can actually produce - `EmIO[IO.EndOfStream | IO.ConnectionReset, A]`.
  */
object EmileError:

  // Payload-free cases are a sealed abstract class with the case object as its sole inhabitant, so
  // type positions name a class. A union arm named by a singleton type mis-erases on Scala 3.8.4 -
  // the TypeTest a typed channel reifies casts its payload to one arm's class, and a value of any
  // other arm then fails its own test.

  /** Failures from `TCP.bind` and `IPC.bind`. */
  sealed trait Bind extends EmileError

  object Bind:
    sealed abstract class AddressInUse private () extends EmileError("Address in use", None) with Bind
    case object AddressInUse extends AddressInUse

    sealed abstract class AddressNotAvailable private () extends EmileError("Address not available", None) with Bind
    case object AddressNotAvailable extends AddressNotAvailable

    sealed abstract class PermissionDenied private () extends EmileError("Permission denied", None) with Bind
    case object PermissionDenied extends PermissionDenied

    /** A bind address emile rejected before libuv - an over-long or empty IPC path, or a filesystem
      * mode requested on an abstract or autobind socket.
      */
    final case class InvalidAddress(detail: String) extends EmileError(s"Invalid address: $detail", None) with Bind

    final case class System(code: ErrorCode) extends EmileError("", None) with Bind:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Bind:
      override def getMessage: String = s"Unexpected bind failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Bind = cause match
        case e: Bind => e
        case t => new Unexpected(t)

      def unapply(u: Unexpected): Some[Throwable] = Some(u.cause)
  end Bind

  /** Failures from `TCP.connect` and `IPC.connect`; through [[HostConnect]] the
    * `TCP.connect(host, port)` overload unifies these with [[DNS]].
    */
  sealed trait Connect extends HostConnect

  object Connect:
    sealed abstract class ConnectionRefused private () extends EmileError("Connection refused", None) with Connect
    case object ConnectionRefused extends ConnectionRefused

    sealed abstract class NetworkUnreachable private () extends EmileError("Network unreachable", None) with Connect
    case object NetworkUnreachable extends NetworkUnreachable

    sealed abstract class HostUnreachable private () extends EmileError("Host unreachable", None) with Connect
    case object HostUnreachable extends HostUnreachable

    sealed abstract class AddressNotAvailable private () extends EmileError("Address not available", None) with Connect
    case object AddressNotAvailable extends AddressNotAvailable

    sealed abstract class TimedOut private () extends EmileError("Connection timed out", None) with Connect
    case object TimedOut extends TimedOut

    sealed abstract class PermissionDenied private () extends EmileError("Permission denied", None) with Connect
    case object PermissionDenied extends PermissionDenied

    sealed abstract class NotFound private () extends EmileError("No such file or directory", None) with Connect
    case object NotFound extends NotFound

    sealed abstract class TooManyOpenFiles private () extends EmileError("Too many open files", None) with Connect
    case object TooManyOpenFiles extends TooManyOpenFiles

    /** A connect argument emile rejected before reaching libuv - for example an
      * [[IPCAddress.Autobind]], which is bind-only.
      */
    final case class InvalidAddress(detail: String) extends EmileError(s"Invalid address: $detail", None) with Connect

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

      def unapply(u: Unexpected): Some[Throwable] = Some(u.cause)
  end Connect

  /** Common parent of [[Connect]] and [[DNS]] - the error type of `TCP.connect(host, port)`. */
  sealed trait HostConnect extends EmileError

  /** Failures from I/O on a live handle - socket reads / writes / half-closes, file open and reads,
    * filesystem-change watching, and fd-readiness.
    */
  sealed trait IO extends EmileError

  object IO:
    sealed abstract class EndOfStream private () extends EmileError("End of stream", None) with IO
    case object EndOfStream extends EndOfStream

    sealed abstract class ConnectionReset private () extends EmileError("Connection reset", None) with IO
    case object ConnectionReset extends ConnectionReset

    sealed abstract class BrokenPipe private () extends EmileError("Broken pipe", None) with IO
    case object BrokenPipe extends BrokenPipe

    sealed abstract class TimedOut private () extends EmileError("Connection timed out", None) with IO
    case object TimedOut extends TimedOut

    sealed abstract class NotFound private () extends EmileError("No such file or directory", None) with IO
    case object NotFound extends NotFound

    sealed abstract class PermissionDenied private () extends EmileError("Permission denied", None) with IO
    case object PermissionDenied extends PermissionDenied

    sealed abstract class TooManyOpenFiles private () extends EmileError("Too many open files", None) with IO
    case object TooManyOpenFiles extends TooManyOpenFiles

    /** An operation attempted after the owning `Resource` released the socket, server, file, or
      * watcher, or after an abortive `closeReset`.
      */
    sealed abstract class AlreadyClosed private () extends EmileError("Resource already closed", None) with IO
    case object AlreadyClosed extends AlreadyClosed

    /** Operations on one socket must be serialised: a second read (or second write) started while
      * one is already in flight fails with this rather than racing the shared per-direction buffer.
      */
    sealed abstract class ConflictingOperation private ()
        extends EmileError("A concurrent operation conflicts with one already in flight on this resource", None)
        with IO
    case object ConflictingOperation extends ConflictingOperation

    /** An operation argument emile rejected before reaching libuv - for example a keep-alive window
      * below one second.
      */
    final case class InvalidArgument(detail: String) extends EmileError(s"Invalid argument: $detail", None) with IO

    final case class System(code: ErrorCode) extends EmileError("", None) with IO:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with IO:
      override def getMessage: String = s"Unexpected I/O failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): IO = cause match
        case e: IO => e
        case t => new Unexpected(t)

      def unapply(u: Unexpected): Some[Throwable] = Some(u.cause)
  end IO

  /** Failures from `DNS.resolve` / `DNS.reverse`. */
  sealed trait DNS extends HostConnect

  object DNS:
    final case class UnknownHost(host: String) extends EmileError(s"Unknown host: $host", None) with DNS

    /** A retryable resolver failure (`EAI_AGAIN`); unlike [[UnknownHost]] the query may succeed if
      * retried.
      */
    final case class TemporaryFailure(host: String) extends EmileError(s"Temporary name-resolution failure: $host", None) with DNS

    final case class System(code: ErrorCode) extends EmileError("", None) with DNS:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with DNS:
      override def getMessage: String = s"Unexpected DNS failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): DNS = cause match
        case e: DNS => e
        case t => new Unexpected(t)

      def unapply(u: Unexpected): Some[Throwable] = Some(u.cause)
  end DNS

  /** Programmer errors and runtime invariants - surfaced through cats-effect's `Throwable` channel
    * by `absolve`, not through the per-operation typed channels.
    */
  sealed trait Runtime extends EmileError

  object Runtime:
    sealed abstract class MissingLibUVPollingSystem private ()
        extends EmileError(
          "LibUVPollingSystem is not installed in this IORuntime. Use EmileIOApp or Emile.runtime.",
          None
        )
        with Runtime
    case object MissingLibUVPollingSystem extends MissingLibUVPollingSystem

    final case class System(code: ErrorCode) extends EmileError("", None) with Runtime:
      override def getMessage: String = ErrorCode.describe(code)

    final class Unexpected private (val cause: Throwable) extends EmileError("", Some(cause)) with Runtime:
      override def getMessage: String = s"Unexpected runtime failure: ${cause.getMessage}"

    object Unexpected:
      def apply(cause: Throwable): Runtime = cause match
        case e: Runtime => e
        case t => new Unexpected(t)

      def unapply(u: Unexpected): Some[Throwable] = Some(u.cause)
  end Runtime
end EmileError

/** Maps a libuv error code to a typed [[EmileError.Bind]], falling through to
  * [[EmileError.Bind.System]] for codes with no dedicated case.
  */
private[emile] object BindMapping:
  def fromCode(code: Int): EmileError.Bind = code match
    case ErrorCode.UV_EADDRINUSE => EmileError.Bind.AddressInUse
    case ErrorCode.UV_EADDRNOTAVAIL => EmileError.Bind.AddressNotAvailable
    case ErrorCode.UV_EACCES | ErrorCode.UV_EPERM => EmileError.Bind.PermissionDenied
    case other => EmileError.Bind.System(ErrorCode(other))

/** Maps a libuv error code to a typed [[EmileError.Connect]], falling through to
  * [[EmileError.Connect.System]] for codes with no dedicated case.
  */
private[emile] object ConnectMapping:
  def fromCode(code: Int): EmileError.Connect = code match
    case ErrorCode.UV_ECONNREFUSED => EmileError.Connect.ConnectionRefused
    case ErrorCode.UV_ENETUNREACH => EmileError.Connect.NetworkUnreachable
    case ErrorCode.UV_EHOSTUNREACH => EmileError.Connect.HostUnreachable
    case ErrorCode.UV_EADDRNOTAVAIL => EmileError.Connect.AddressNotAvailable
    case ErrorCode.UV_ETIMEDOUT => EmileError.Connect.TimedOut
    case ErrorCode.UV_EACCES | ErrorCode.UV_EPERM => EmileError.Connect.PermissionDenied
    case ErrorCode.UV_ENOENT => EmileError.Connect.NotFound
    case ErrorCode.UV_EMFILE | ErrorCode.UV_ENFILE => EmileError.Connect.TooManyOpenFiles
    case other => EmileError.Connect.System(ErrorCode(other))

/** Maps a libuv error code to a typed [[EmileError.IO]], falling through to
  * [[EmileError.IO.System]] for codes with no dedicated case.
  */
private[emile] object IOMapping:
  def fromCode(code: Int): EmileError.IO = code match
    case ErrorCode.UV_EOF => EmileError.IO.EndOfStream
    case ErrorCode.UV_ECONNRESET => EmileError.IO.ConnectionReset
    case ErrorCode.UV_EPIPE => EmileError.IO.BrokenPipe
    case ErrorCode.UV_ETIMEDOUT => EmileError.IO.TimedOut
    case ErrorCode.UV_EACCES | ErrorCode.UV_EPERM => EmileError.IO.PermissionDenied
    case ErrorCode.UV_ENOENT => EmileError.IO.NotFound
    case ErrorCode.UV_EMFILE | ErrorCode.UV_ENFILE => EmileError.IO.TooManyOpenFiles
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
