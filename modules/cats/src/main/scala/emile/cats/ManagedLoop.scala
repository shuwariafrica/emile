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

// scalafix:off DisableSyntax.null, DisableSyntax.var, DisableSyntax.asInstanceOf, DisableSyntax.while; dedicated loop thread with blocking I/O

import java.util.concurrent.CountDownLatch
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.Resource

import boilerplate.effect.Eff

import emile.Async
import emile.EmileError
import emile.Loop
import emile.LoopConfig
import emile.Open
import emile.RunMode

/** A standalone libuv loop with its own dedicated thread.
  *
  * Unlike `EmileLoop.integrated` which uses the runtime's shared loop, `ManagedLoop` creates a
  * completely independent loop running on its own thread. This is useful for:
  *
  *   - Testing libuv functionality outside the runtime
  *   - Dedicated I/O threads with different priority
  *   - Isolation from the main compute pool
  *
  * ==Thread Model==
  *
  * The loop thread polls the task queue with a short timeout between `uv_run(NoWait)` calls. This
  * ensures responsive task handling while still processing I/O events efficiently.
  *
  * ==Usage==
  *
  * {{{
  * ManagedLoop.create.use { managed =>
  *   for
  *     // Submit work to the loop thread
  *     _ <- managed.submit { loop =>
  *       Timer.after(loop, Timeout.millis(100))(() => println("Timer fired!"))
  *     }
  *     // Run until all handles are closed
  *     _ <- managed.runUntilComplete
  *   yield ()
  * }
  * }}}
  */
final class ManagedLoop private (
  private val loop: Loop,
  private val thread: Thread,
  private val taskQueue: LinkedBlockingQueue[ManagedLoop.Task],
  private val running: AtomicBoolean,
  private val asyncHandle: Async[Open]
):

  /** The Eff type alias for this module. */
  private type F[A] = Eff[IO, EmileError, A]

  /** Submit a task to be executed on the loop thread.
    *
    * The task will be executed during the next loop iteration.
    *
    * @param f The task to execute, receives the loop and returns Either
    * @return Eff that completes with the result or error
    */
  def submit[A](f: Loop => Either[EmileError, A]): F[A] =
    Eff.lift[IO, EmileError, A](
      IO.async_[Either[EmileError, A]] { cb =>
        val task = ManagedLoop.Task.Execute(
          loop => f(loop),
          result => cb(Right(result.asInstanceOf[Either[EmileError, A]]))
        )
        taskQueue.put(task)
        // Wake up the loop if it's blocked
        val _ = asyncHandle.send
      }
    )

  /** Submit a pure task to be executed on the loop thread.
    *
    * @param f The task to execute, receives the loop
    * @return Eff that completes with the result
    */
  def submitPure[A](f: Loop => A): F[A] =
    Eff.liftF[IO, EmileError, A](
      IO.async_[A] { cb =>
        val task = ManagedLoop.Task.Execute(
          loop => f(loop),
          result => cb(Right(result.asInstanceOf[A]))
        )
        taskQueue.put(task)
        val _ = asyncHandle.send
      }
    )

  /** Run the loop until all user handles are closed.
    *
    * The async handle used for internal signalling is excluded from the active handle count.
    *
    * @return Eff that completes when all user handles are drained
    */
  def runUntilComplete: F[Unit] =
    Eff.liftF[IO, EmileError, Unit] {
      val latch = new CountDownLatch(1)
      val checkTask = ManagedLoop.Task.RunUntilComplete(latch)
      taskQueue.put(checkTask)
      val _ = asyncHandle.send
      IO.blocking(latch.await())
    }

  /** Get the underlying loop for direct use.
    *
    * '''Warning:''' Only use this within a `submit` callback to ensure thread safety. Accessing the
    * loop from other threads is undefined behaviour.
    */
  def unsafeLoop: Loop = loop

end ManagedLoop

object ManagedLoop:

  /** The Eff type alias for this module. */
  private type F[A] = Eff[IO, EmileError, A]

  /** Tasks submitted to the loop thread. */
  sealed private trait Task
  private object Task:
    final case class Execute(f: Loop => Any, cb: Any => Unit) extends Task
    final case class RunUntilComplete(latch: CountDownLatch) extends Task
    case object Shutdown extends Task

  /** CanEqual instance for Task pattern matching under strict equality. */
  @scala.annotation.nowarn("msg=unused private member")
  private given CanEqual[Task, Task] = CanEqual.derived

  /** Create a managed loop with default configuration.
    *
    * @return Resource that manages the loop and its thread lifecycle
    */
  def create: Resource[Eff.Of[IO, EmileError], ManagedLoop] =
    create(LoopConfig.empty)

  /** Create a managed loop with custom configuration.
    *
    * @param config Loop configuration options
    * @return Resource that manages the loop and its thread lifecycle
    */
  def create(config: LoopConfig): Resource[Eff.Of[IO, EmileError], ManagedLoop] =
    Resource.make(acquire(config))(release)

  private def acquire(config: LoopConfig): F[ManagedLoop] =
    Eff.lift[IO, EmileError, ManagedLoop](
      IO.async_[Either[EmileError, ManagedLoop]] { cb =>
        val taskQueue = new LinkedBlockingQueue[Task]()
        val running = new AtomicBoolean(true)
        val startupLatch = new CountDownLatch(1)
        val resultRef = new AtomicReference[Either[EmileError, ManagedLoop]](null)

        val loopThread = new Thread:
          self =>
          override def run(): Unit =
            // Create loop on this thread
            Loop.create(config) match
              case Left(err) =>
                resultRef.set(Left(err))
                startupLatch.countDown()

              case Right(loop) =>
                // Create async handle for waking up the loop
                Async.init(loop) { () =>
                  () // Async callback just wakes up the loop; queue is drained in main loop
                } match
                  case Left(err) =>
                    val _ = loop.close
                    resultRef.set(Left(err))
                    startupLatch.countDown()

                  case Right(asyncHandle) =>
                    val managed = new ManagedLoop(loop, self, taskQueue, running, asyncHandle)
                    resultRef.set(Right(managed))
                    startupLatch.countDown()

                    // Main loop - poll queue and run I/O
                    try
                      while running.get() do
                        val _ = loop.run(RunMode.NoWait)
                        drainQueue(loop, taskQueue, running, asyncHandle)
                        if running.get() then
                          val task = taskQueue.poll(10, TimeUnit.MILLISECONDS)
                          if task != null then processTask(loop, task, running, asyncHandle)
                    catch case _: InterruptedException => () // scalafix:ok; shutdown requested

                    // Cleanup: close all handles and drain the loop
                    asyncHandle.closeAsync(_ => ())
                    loop.walkAndClose()
                    while loop.isAlive do
                      val _ = loop.run(RunMode.NoWait)
                    val _ = loop.close

        loopThread.setName("emile-managed-loop")
        loopThread.setDaemon(true)
        loopThread.start()

        // Wait for startup to complete
        startupLatch.await()
        cb(Right(resultRef.get()))
      }
    )

  private def drainQueue(
    loop: Loop,
    queue: LinkedBlockingQueue[Task],
    running: AtomicBoolean,
    asyncHandle: Async[Open]
  ): Unit =
    var task = queue.poll()
    while task != null do
      processTask(loop, task, running, asyncHandle)
      task = queue.poll()

  private def processTask(
    loop: Loop,
    task: Task,
    running: AtomicBoolean,
    asyncHandle: Async[Open]
  ): Unit =
    task match
      case exec: Task.Execute =>
        val result = exec.f(loop)
        exec.cb(result)

      case ruc: Task.RunUntilComplete =>
        runUntilUserHandlesDrained(loop, ruc.latch, asyncHandle, running)

      case Task.Shutdown =>
        running.set(false)

  private def runUntilUserHandlesDrained(
    loop: Loop,
    latch: CountDownLatch,
    asyncHandle: Async[Open],
    running: AtomicBoolean
  ): Unit =
    asyncHandle.unref

    // Run until no referenced user handles remain or shutdown requested
    while loop.isAlive && running.get() do
      val _ = loop.run(RunMode.Once)

    asyncHandle.ref
    latch.countDown()
  end runUntilUserHandlesDrained

  private def release(managed: ManagedLoop): F[Unit] =
    Eff.liftF[IO, EmileError, Unit](
      IO.blocking {
        managed.running.set(false)
        managed.taskQueue.put(Task.Shutdown)
        val _ = managed.asyncHandle.send
        // Cooperative shutdown: running flag is checked in the main loop
        // and in runUntilUserHandlesDrained. After the flag is set, the
        // thread exits the main loop within ~10ms (queue poll timeout),
        // runs walkAndClose to drain handles, and terminates.
        managed.thread.join()
      }
    )

end ManagedLoop
