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

import scala.annotation.targetName

import cats.effect.IO
import cats.effect.kernel.Async
import cats.syntax.all.*

import boilerplate.effect.*

import emile.EmileError
import emile.Loop

/** Serialized libuv loop access for cats-effect integration.
  *
  * All primitives that touch libuv acquire exclusive access to the shared loop via a CAS flag
  * (`loopBusy`). When contention exists (another worker is in `uv_run`), operations are queued and
  * executed by the holding worker.
  *
  * Every primitive that needs the loop receives it as a parameter to the callback — consumers never
  * need to acquire the loop themselves.
  *
  * This is package-private to `emile.cats`.
  */
private[cats] object EffAsync:
  private type F[A] = Eff[IO, EmileError, A]

  // =========================================================================
  // Core: serialized loop access
  // =========================================================================

  /** Execute a synchronous libuv operation with exclusive loop access.
    *
    * @param f Receives the shared loop, returns Either result.
    */
  def onLoop[A](f: Loop => Either[EmileError, A]): F[A] =
    LibuvPollingSystem.LoopAccess.get.flatMap { access =>
      val system = access.system
      Eff.lift[IO, EmileError, A](
        IO.defer {
          if system.loopBusy.compareAndSet(false, true) then
            try IO.pure(f(system.loop))
            finally system.loopBusy.set(false)
          else
            IO.async[Either[EmileError, A]] { cb =>
              IO {
                system.submitTask(() => cb(Right(f(system.loop))))
                None
              }
            }
        }
      )
    }

  // =========================================================================
  // Handle close
  // =========================================================================

  /** Close a libuv handle and wait for the close callback.
    *
    * @param closeAsync The handle's `closeAsync` method
    */
  def closeHandle(closeAsync: (Either[EmileError, Unit] => Unit) => Unit): F[Unit] =
    LibuvPollingSystem.LoopAccess.get.flatMap { access =>
      val system = access.system
      Eff.liftF[IO, EmileError, Unit](
        IO.async_[Unit] { cb =>
          val task: Runnable = () => closeAsync(_ => cb(Right(())))
          if system.loopBusy.compareAndSet(false, true) then
            try task.run()
            finally system.loopBusy.set(false)
          else system.submitTask(task)
        }
      )
    }

  // =========================================================================
  // Async primitives WITH pending operations tracking
  // =========================================================================

  /** Register an async libuv callback with pending operations tracking.
    *
    * The registration function receives the shared loop and runs with exclusive loop access. It
    * must call `complete` exactly once.
    *
    * @param register `(loop, complete) => Unit` — perform libuv FFI calls using `loop`, call
    *   `complete` when the callback fires.
    */
  def asyncWithPending[A](register: (Loop, Either[EmileError, A] => Unit) => Unit): F[A] =
    LibuvPollingSystem.LoopAccess.get.flatMap { access =>
      val system = access.system
      Eff.lift[IO, EmileError, A](
        IO.async[Either[EmileError, A]] { cb =>
          IO.async_[Option[IO[Unit]]] { setupCb =>
            val task: Runnable = () =>
              access.accessPoller { p =>
                val decremented = new java.util.concurrent.atomic.AtomicBoolean(false)
                p.incrementPending()

                register(system.loop,
                         { result =>
                           if decremented.compareAndSet(false, true) then p.decrementPending()
                           cb(Right(result))
                         }
                )

                val finalizer = IO {
                  if decremented.compareAndSet(false, true) then p.decrementPending()
                }
                setupCb(Right(Some(finalizer)))
              }

            if system.loopBusy.compareAndSet(false, true) then
              try task.run()
              finally system.loopBusy.set(false)
            else system.submitTask(task)
          }
        }
      )
    }

  /** Unit-returning version of asyncWithPending. */
  def asyncWithPendingUnit(register: (Loop, Either[EmileError, Unit] => Unit) => Unit): F[Unit] =
    asyncWithPending[Unit](register)

  /** Register an async libuv callback with pending tracking and cancellation.
    *
    * The registration function receives the shared loop and runs with exclusive loop access. It
    * returns an optional cancellation finalizer.
    *
    * @param register `(loop, complete) => F[Option[F[Unit]]]`
    */
  def asyncWithPendingCancellable[A](
    register: (Loop, Either[EmileError, A] => Unit) => F[Option[F[Unit]]]
  ): F[A] =
    LibuvPollingSystem.LoopAccess.get.flatMap { access =>
      val system = access.system
      Eff
        .liftF[IO, EmileError, Unit](
          IO.async_[Unit](cb => access.accessPoller(_ => cb(Right(()))))
        )
        .flatMap { _ =>
          val pollerRef = new java.util.concurrent.atomic.AtomicReference[LibuvPollingSystem.LibuvPoller]
          val decremented = new java.util.concurrent.atomic.AtomicBoolean(false)
          access.accessPoller { p =>
            pollerRef.set(p)
            p.incrementPending()
          }

          Async[F]
            .async[Either[EmileError, A]] { cb =>
              val registered =
                if system.loopBusy.compareAndSet(false, true) then
                  try
                    register(
                      system.loop,
                      { result =>
                        if decremented.compareAndSet(false, true) then
                          val p = pollerRef.get()
                          if p != null then p.decrementPending() // scalafix:ok
                        cb(Right(result))
                      }
                    )
                  finally
                    system.loopBusy.set(false)
                else
                  register(
                    system.loop,
                    { result =>
                      if decremented.compareAndSet(false, true) then
                        val p = pollerRef.get()
                        if p != null then p.decrementPending() // scalafix:ok
                      cb(Right(result))
                    }
                  )

              registered.map { optFinalizer =>
                val decrementIO = IO {
                  if decremented.compareAndSet(false, true) then
                    val p = pollerRef.get()
                    if p != null then p.decrementPending() // scalafix:ok
                }
                Some(Eff.liftF[IO, EmileError, Unit](decrementIO) *> optFinalizer.getOrElse(Eff.unit[IO, EmileError]))
              }
            }
            .subflatMap(identity)
        }
    }

  // =========================================================================
  // Async primitives WITHOUT pending tracking (non-libuv)
  // =========================================================================

  @targetName("asyncUnit")
  def async_[A](register: (Either[EmileError, A] => Unit) => Unit): F[A] =
    Async[F]
      .async_[Either[EmileError, A]] { cb =>
        register(result => cb(Right(result)))
      }
      .subflatMap(identity)

  def async_Unit(register: (Either[EmileError, Unit] => Unit) => Unit): F[Unit] =
    async_[Unit](register)

  def async[A](register: (Either[EmileError, A] => Unit) => F[Option[F[Unit]]]): F[A] =
    Async[F]
      .async[Either[EmileError, A]] { cb =>
        register(result => cb(Right(result)))
      }
      .subflatMap(identity)

  // =========================================================================
  // Synchronous effect primitives
  // =========================================================================

  def delay[A](thunk: => A): F[A] =
    Async[F].delay(thunk)

  def blocking[A](thunk: => A): F[A] =
    Async[F].blocking(thunk)

  def sleep(duration: scala.concurrent.duration.FiniteDuration): F[Unit] =
    cats.effect.kernel.Temporal[F].sleep(duration)

end EffAsync
