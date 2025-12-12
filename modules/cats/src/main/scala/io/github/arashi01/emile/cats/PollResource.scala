/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.cats

import cats.effect.{IO, Resource}
import scala.scalanative.unsafe.Ptr
import io.github.arashi01.emile.{EmileError, Loop, Open, Poll, PollEvent}

/**
 * cats-effect Resource integration for Poll handles.
 *
 * Poll handles are used to watch file descriptors for readability,
 * writability and disconnection, similar to POSIX `poll(2)`.
 */
object PollResource:

  /** Helper to lift Either[EmileError, A] to IO[A]. */
  private def liftEmile[A](either: Either[EmileError, A]): IO[A] =
    either.fold(e => IO.raiseError(e), IO.pure)

  /**
   * Create a poll handle as a managed resource.
   *
   * The handle is closed asynchronously when the resource is released.
   * The finalizer awaits the close callback before returning.
   *
   * @param fd The file descriptor to poll
   * @param loop The event loop (implicit)
   * @return Resource that acquires and safely releases a poll handle
   */
  def make(fd: Int)(using loop: Loop): Resource[IO, Poll[Open]] =
    Resource.make(
      acquire = liftEmile(Poll.init(loop, fd))
    )(
      release = poll => IO.async_ { cb =>
        poll.closeAsync(_ => cb(Right(())))
      }
    )

  /**
   * Create a poll handle for a socket as a managed resource.
   *
   * On Unix this is identical to `make`. On Windows this accepts
   * a SOCKET handle instead of a file descriptor.
   *
   * @param socket The socket to poll (platform-specific type)
   * @param loop The event loop (implicit)
   * @return Resource that acquires and safely releases a poll handle
   */
  def makeSocket(socket: Ptr[Byte])(using loop: Loop): Resource[IO, Poll[Open]] =
    Resource.make(
      acquire = liftEmile(Poll.initSocket(loop, socket))
    )(
      release = poll => IO.async_ { cb =>
        poll.closeAsync(_ => cb(Right(())))
      }
    )

  /**
   * Create a poll handle and start polling for events.
   *
   * This is a convenience method combining resource allocation and poll start.
   *
   * @param fd The file descriptor to poll
   * @param events The events to poll for
   * @param callback Callback invoked when events are detected
   * @param loop The event loop (implicit)
   * @return Resource that acquires a started poll handle
   */
  def started(fd: Int, events: PollEvent*)(callback: (Int, Set[PollEvent]) => Unit)(using loop: Loop): Resource[IO, Poll[Open]] =
    make(fd).evalTap { poll =>
      liftEmile(poll.start(events*)(callback))
    }
end PollResource
