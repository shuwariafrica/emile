/*
 * Copyright 2025, 2026 Ali Rashid.
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
package emile.cats

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*

import boilerplate.effect.*
import boilerplate.effect.Eff

import emile.EmileError
import emile.ErrorCode
import emile.Open
import emile.Tcp
import emile.TcpConfig
import emile.ipa.SocketAddress

/** cats-effect Resource integration for TCP handles.
  *
  * Provides managed resource acquisition and safe async cleanup for TCP handles.
  */
object TcpResource:

  /** Close TCP handle with serialized loop access. */
  private def closeAsyncEff(tcp: Tcp[Open]): Eff[IO, EmileError, Unit] =
    EffAsync.closeHandle(tcp.closeAsync)

  /** Create a TCP handle as a managed resource.
    *
    * The handle is closed asynchronously when the resource is released. The finalizer awaits the
    * close callback before returning.
    *
    * @return Resource that acquires and safely releases a TCP handle with typed error channel
    */
  def make: Resource[Eff.Of[IO, EmileError], Tcp[Open]] =
    Resource.make(
      acquire = acquireTcp(None)
    )(
      release = closeAsyncEff
    )

  /** Create a TCP handle with configuration as a managed resource.
    *
    * @param config Configuration for the TCP handle
    * @return Resource that acquires and safely releases a TCP handle with typed error channel
    */
  def make(config: TcpConfig): Resource[Eff.Of[IO, EmileError], Tcp[Open]] =
    Resource.make(
      acquire = acquireTcp(Some(config))
    )(
      release = closeAsyncEff
    )

  /** Create a TCP server socket bound to an address.
    *
    * This is a convenience method combining handle creation and bind.
    *
    * @param address The address to bind to
    * @return Resource that acquires a bound TCP handle with typed error channel
    */
  def bind(address: SocketAddress): Resource[Eff.Of[IO, EmileError], Tcp[Open]] =
    make.evalTap { tcp =>
      tcp.bind(address).eff[IO]
    }

  /** Create a TCP server socket bound to an address with configuration.
    *
    * @param address The address to bind to
    * @param config Configuration for the TCP handle
    * @return Resource that acquires a bound TCP handle with typed error channel
    */
  def bind(address: SocketAddress, config: TcpConfig): Resource[Eff.Of[IO, EmileError], Tcp[Open]] =
    make(config).evalTap { tcp =>
      tcp.bind(address).eff[IO]
    }

  /** Create a TCP client and connect to a remote address.
    *
    * The connection is completed asynchronously. The resource is released when the TCP handle is
    * closed.
    *
    * @param address The remote address to connect to
    * @return Resource that acquires a connected TCP handle with typed error channel
    */
  def connect(address: SocketAddress): Resource[Eff.Of[IO, EmileError], Tcp[Open]] =
    Resource.make(
      acquire = acquireTcp(None).flatMap(tcp => connectEff(tcp, address))
    )(release = closeAsyncEff)

  /** Create a TCP client with configuration and connect to a remote address.
    *
    * @param address The remote address to connect to
    * @param config Configuration for the TCP handle
    * @return Resource that acquires a connected TCP handle with typed error channel
    */
  def connect(address: SocketAddress, config: TcpConfig): Resource[Eff.Of[IO, EmileError], Tcp[Open]] =
    Resource.make(
      acquire = acquireTcp(Some(config)).flatMap(tcp => connectEff(tcp, address))
    )(release = closeAsyncEff)

  private inline def acquireTcp(config: Option[TcpConfig]): Eff[IO, EmileError, Tcp[Open]] =
    EffAsync.onLoop(loop => config.fold(Tcp.init(loop))(Tcp.init(loop, _)))

  /** Connect to address, returning result in Eff channel (typed errors). */
  private def connectEff(tcp: Tcp[Open], address: SocketAddress): Eff[IO, EmileError, Tcp[Open]] =
    EffAsync
      .asyncWithPendingCancellable[Tcp[Open]] { (_, complete) =>
        tcp.connect(address) { status =>
          if status >= 0 then complete(Right(tcp))
          else complete(Left(EmileError.fromErrorCode(ErrorCode(status))))
        } match
          case Right(_) =>
            Eff.succeed[IO, EmileError, Option[Eff[IO, EmileError, Unit]]](Some(closeAsyncEff(tcp)))
          case Left(e) =>
            tcp.closeAsync(_ => ()): Unit
            complete(Left(e))
            Eff.succeed[IO, EmileError, Option[Eff[IO, EmileError, Unit]]](None)
      }
      .onError { case _ => closeAsyncEff(tcp).void }
end TcpResource
