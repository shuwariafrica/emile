/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import zio.*
import io.github.arashi01.emile.{EmileError, ErrorCode, Open, Tcp, TcpConfig}
import io.github.arashi01.emile.ipa.SocketAddress

/**
 * ZIO Scope integration for TCP handles.
 *
 * Provides scoped resource acquisition and safe async cleanup for TCP handles.
 * Handles are closed asynchronously when the enclosing scope closes.
 */
object TcpResource:
  /**
   * Create a TCP handle managed by the current Scope.
   *
   * The handle is closed asynchronously when the scope closes.
   */
  inline def make: ZIO[EmileLoop & Scope, EmileError, Tcp[Open]] =
    for
      loop <- EmileLoop.loop
      tcp <- ZIO.acquireRelease(
        acquire = ZIO.fromEither(Tcp.init(loop))
      )(
        release = handle => closeAsync(handle)
      )
    yield tcp

  /**
   * Create a TCP handle with configuration, managed by the current Scope.
   *
   * @param config Configuration for the TCP handle
   */
  inline def make(config: TcpConfig): ZIO[EmileLoop & Scope, EmileError, Tcp[Open]] =
    for
      loop <- EmileLoop.loop
      tcp <- ZIO.acquireRelease(
        acquire = ZIO.fromEither(Tcp.init(loop, config))
      )(
        release = handle => closeAsync(handle)
      )
    yield tcp

  /**
   * Create a TCP server socket bound to an address.
   *
   * Combines handle creation and bind in a single scoped operation.
   *
   * @param address The address to bind to
   */
  def bind(address: SocketAddress): ZIO[EmileLoop & Scope, EmileError, Tcp[Open]] =
    make.flatMap { tcp =>
      ZIO.fromEither(tcp.bind(address)).as(tcp)
    }

  /**
   * Create a TCP server socket bound to an address with configuration.
   *
   * @param address The address to bind to
   * @param config Configuration for the TCP handle
   */
  def bind(address: SocketAddress, config: TcpConfig): ZIO[EmileLoop & Scope, EmileError, Tcp[Open]] =
    make(config).flatMap { tcp =>
      ZIO.fromEither(tcp.bind(address, config)).as(tcp)
    }

  /**
   * Create a TCP client and connect to a remote address.
   *
   * The connection is completed asynchronously.
   *
   * @param address The remote address to connect to
   */
  def connect(address: SocketAddress): ZIO[EmileLoop & Scope, EmileError, Tcp[Open]] =
    make.flatMap { tcp =>
      connectAsync(tcp, address).as(tcp)
    }

  /**
   * Create a TCP client with configuration and connect to a remote address.
   *
   * @param address The remote address to connect to
   * @param config Configuration for the TCP handle
   */
  def connect(address: SocketAddress, config: TcpConfig): ZIO[EmileLoop & Scope, EmileError, Tcp[Open]] =
    make(config).flatMap { tcp =>
      connectAsync(tcp, address).as(tcp)
    }

  // ===========================================================================
  // Internal async helpers
  // ===========================================================================

  /**
   * Close handle asynchronously, awaiting callback.
   *
   * Note: Ignores AlreadyClosed errors since handles may be closed
   * by the loop's walkAndClose during finalization.
   */
  private def closeAsync(handle: Tcp[Open]): UIO[Unit] =
    ZIO.async[Any, Nothing, Unit] { cb =>
      handle.closeAsync(_ => cb(ZIO.unit))
    }

  /** Connect asynchronously, awaiting callback. */
  private def connectAsync(tcp: Tcp[Open], address: SocketAddress): IO[EmileError, Unit] =
    ZIO.async[Any, EmileError, Unit] { cb =>
      tcp.connect(address) { status =>
        if status >= 0 then cb(ZIO.unit)
        else cb(ZIO.fail(EmileError.fromErrorCode(ErrorCode(status))))
      } match
        case Right(_) => ()
        case Left(e)  => cb(ZIO.fail(e))
    }
end TcpResource
