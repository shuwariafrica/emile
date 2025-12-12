/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.{IO, Resource}
import io.github.arashi01.emile.{EmileError, ErrorCode, Loop, Open, Tcp, TcpConfig}
import io.github.arashi01.emile.ipa.SocketAddress

/**
 * cats-effect Resource integration for TCP handles.
 *
 * Provides managed resource acquisition and safe async cleanup for TCP handles.
 */
object TcpResource:

  /** Helper to lift Either[EmileError, A] to IO[A]. */
  private def liftEmile[A](either: Either[EmileError, A]): IO[A] =
    either.fold(e => IO.raiseError(e), IO.pure)

  /**
   * Create a TCP handle as a managed resource.
   *
   * The handle is closed asynchronously when the resource is released.
   * The finalizer awaits the close callback before returning.
   *
   * @param loop The event loop (implicit)
   * @return Resource that acquires and safely releases a TCP handle
   */
  def make(using loop: Loop): Resource[IO, Tcp[Open]] =
    Resource.make(
      acquire = liftEmile(Tcp.init(loop))
    )(
      release = tcp => IO.async_ { cb =>
        tcp.closeAsync(_ => cb(Right(())))
      }
    )

  /**
   * Create a TCP handle with configuration as a managed resource.
   *
   * @param config Configuration for the TCP handle
   * @param loop The event loop (implicit)
   * @return Resource that acquires and safely releases a TCP handle
   */
  def make(config: TcpConfig)(using loop: Loop): Resource[IO, Tcp[Open]] =
    Resource.make(
      acquire = liftEmile(Tcp.init(loop, config))
    )(
      release = tcp => IO.async_ { cb =>
        tcp.closeAsync(_ => cb(Right(())))
      }
    )

  /**
   * Create a TCP server socket bound to an address.
   *
   * This is a convenience method combining handle creation and bind.
   *
   * @param address The address to bind to
   * @param loop The event loop (implicit)
   * @return Resource that acquires a bound TCP handle
   */
  def bind(address: SocketAddress)(using loop: Loop): Resource[IO, Tcp[Open]] =
    make.evalTap { tcp =>
      liftEmile(tcp.bind(address))
    }

  /**
   * Create a TCP server socket bound to an address with configuration.
   *
   * @param address The address to bind to
   * @param config Configuration for the TCP handle
   * @param loop The event loop (implicit)
   * @return Resource that acquires a bound TCP handle
   */
  def bind(address: SocketAddress, config: TcpConfig)(using loop: Loop): Resource[IO, Tcp[Open]] =
    make(config).evalTap { tcp =>
      liftEmile(tcp.bind(address))
    }

  /**
   * Create a TCP client and connect to a remote address.
   *
   * The connection is completed asynchronously. The resource is released
   * when the TCP handle is closed.
   *
   * @param address The remote address to connect to
   * @param loop The event loop (implicit)
   * @return Resource that acquires a connected TCP handle
   */
  def connect(address: SocketAddress)(using loop: Loop): Resource[IO, Tcp[Open]] =
    make.evalTap { tcp =>
      IO.async_[Unit] { cb =>
        tcp.connect(address) { status =>
          if status >= 0 then cb(Right(()))
          else cb(Left(EmileError.fromErrorCode(ErrorCode(status))))
        } match
          case Right(_) => ()
          case Left(e)  => cb(Left(e))
      }
    }

  /**
   * Create a TCP client with configuration and connect to a remote address.
   *
   * @param address The remote address to connect to
   * @param config Configuration for the TCP handle
   * @param loop The event loop (implicit)
   * @return Resource that acquires a connected TCP handle
   */
  def connect(address: SocketAddress, config: TcpConfig)(using loop: Loop): Resource[IO, Tcp[Open]] =
    make(config).evalTap { tcp =>
      IO.async_[Unit] { cb =>
        tcp.connect(address) { status =>
          if status >= 0 then cb(Right(()))
          else cb(Left(EmileError.fromErrorCode(ErrorCode(status))))
        } match
          case Right(_) => ()
          case Left(e)  => cb(Left(e))
      }
    }
end TcpResource
