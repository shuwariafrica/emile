/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import scala.scalanative.unsafe.*
import scala.scalanative.libc.stdlib
import io.github.arashi01.emile.unsafe.LibUV
import io.github.arashi01.emile.unsafe.types.UvLoopPtr

/**
 * Handle to a libuv event loop.
 *
 * Manages the lifecycle of all handles and drives I/O operations.
 * The loop is the central structure in libuv, processing events and
 * dispatching callbacks.
 */
opaque type Loop = Ptr[Byte]

object Loop:
  given CanEqual[Loop, Loop] = CanEqual.derived

  // Loop configuration option constants from libuv
  private val UV_LOOP_BLOCK_SIGNAL = 0
  private val UV_METRICS_IDLE_TIME = 1
  private val UV_LOOP_USE_IO_URING_SQPOLL = 2

  /**
   * Obtain the default global event loop.
   *
   * The default loop is a global singleton managed by libuv.
   * It should NOT be closed by the application.
   *
   * @return Right with the default loop, or Left on error
   */
  def default: Either[EmileError, Loop] =
    val loop = LibUV.uv_default_loop()
    if loop == null then Left(EmileError.SystemError(ErrorCode.NoMemory, "Failed to get default loop"))
    else Right(loop)

  /**
   * Create a new event loop with libuv defaults.
   *
   * The caller is responsible for closing the loop when done.
   * This is the most efficient path when no configuration overrides are needed.
   *
   * @return Right with new loop, or Left on error
   */
  def create: Either[EmileError, Loop] =
    val size = LibUV.uv_loop_size()
    val loop = stdlib.calloc(1L, size.toLong)
    if loop == null then Left(EmileError.SystemError(ErrorCode.NoMemory, "Failed to allocate loop"))
    else
      val result = LibUV.uv_loop_init(loop)
      if result < 0 then
        stdlib.free(loop)
        Left(toSystemError(result))
      else
        Right(loop)

  /**
   * Create a new event loop with the specified configuration overrides.
   *
   * Only values explicitly set in the config are applied - unset values
   * use libuv's built-in defaults.
   *
   * The caller is responsible for closing the loop when done.
   *
   * @param config Configuration overrides for the loop
   * @return Right with new loop, or Left on error
   */
  def create(config: LoopConfig): Either[EmileError, Loop] =
    // Fast path: no overrides = use direct create
    if !config.hasOverrides then create
    else
      val size = LibUV.uv_loop_size()
      val loop = stdlib.calloc(1L, size.toLong)
      if loop == null then Left(EmileError.SystemError(ErrorCode.NoMemory, "Failed to allocate loop"))
      else
        val result = LibUV.uv_loop_init(loop)
        if result < 0 then
          stdlib.free(loop)
          Left(toSystemError(result))
        else
          // Apply only specified configuration overrides
          applyConfig(loop, config) match
            case Left(err) =>
              val _ = LibUV.uv_loop_close(loop)
              stdlib.free(loop)
              Left(err)
            case Right(_) =>
              Right(loop)

  /**
   * Apply configuration overrides to an existing loop.
   *
   * Only values explicitly set in the config are applied.
   *
   * Note: Loop configuration should normally be done before the first
   * call to `run`. Some options may not take effect if applied later.
   */
  private def applyConfig(loop: Ptr[Byte], config: LoopConfig): Either[EmileError, Unit] =
    /** Configure a loop option, returning Right(()) on success or if unsupported (ENOSYS) */
    def configure(option: CInt, value: CInt): Either[EmileError, Unit] =
      val result = LibUV.uv_loop_configure(loop, option, value)
      if result < 0 && result != ErrorCode.NoSys.value then Left(toSystemError(result))
      else Right(())

    for
      // Apply metrics idle time if specified and enabled
      _ <- config.metricsEnabled match
        case Some(true) => configure(UV_METRICS_IDLE_TIME, 0)
        case _ => Right(())
      // Apply signal blocking if specified
      _ <- config.blockSignal.fold(Right(()): Either[EmileError, Unit])(signal => configure(UV_LOOP_BLOCK_SIGNAL, signal))
      // Apply io_uring SQPOLL if specified and enabled (Linux only)
      _ <- config.useIoUringSqpoll match
        case Some(true) => configure(UV_LOOP_USE_IO_URING_SQPOLL, 0)
        case _ => Right(())
    yield ()

  extension (loop: Loop)
    /**
     * Run the event loop with the specified mode.
     *
     * @param mode The run mode (Default, Once, or NoWait)
     * @return Right with true if loop still has active handles, Left on error
     */
    def run(mode: RunMode): Either[EmileError, Boolean] =
      val result = LibUV.uv_run(loop, mode.toLibuv)
      // uv_run returns non-zero if loop is still alive (has pending events)
      Right(result != 0)

    /**
     * Stop the event loop.
     *
     * Causes `run` to return as soon as possible. This will happen no sooner
     * than the next loop iteration. If called before blocking for I/O, the
     * loop won't block for I/O on this iteration.
     */
    def stop: Unit =
      LibUV.uv_stop(loop)

    /**
     * Check if the loop has active handles or requests.
     *
     * @return true if there are active handles/requests
     */
    def isAlive: Boolean =
      LibUV.uv_loop_alive(loop) != 0

    /**
     * Get current cached timestamp in milliseconds.
     *
     * The timestamp is cached at the start of the event loop tick.
     * Use `updateTime` to refresh it if needed.
     *
     * @return Current cached timestamp
     */
    def now: Timestamp =
      Timestamp(LibUV.uv_now(loop).toLong)

    /**
     * Update the loop's cached timestamp.
     *
     * libuv caches the current time at the start of the event loop tick.
     * This avoids excessive syscalls. Call this to force a time update.
     */
    def updateTime: Unit =
      LibUV.uv_update_time(loop)

    /**
     * Close the event loop.
     *
     * All handles must be closed before calling this.
     *
     * NOTE: Do not call this on the default loop.
     *
     * @return Right on success, Left on error (e.g., handles still active)
     */
    def close: Either[EmileError, Unit] =
      // Check if this is the default loop - don't free it as libuv owns it
      val defaultLoop = LibUV.uv_default_loop()
      val isDefault = loop == defaultLoop
      
      val result = LibUV.uv_loop_close(loop)
      if result < 0 then Left(toSystemError(result))
      else
        // Only free if not the default loop
        if !isDefault then stdlib.free(loop)
        Right(())

    /**
     * Walk all handles and initiate close on each.
     *
     * This is the first step of async loop cleanup. After calling this,
     * continue running the loop until `isAlive` returns false, then
     * call `close` to release the loop memory.
     *
     * Does nothing for the default loop.
     */
    def walkAndClose(): Unit =
      // Check if this is the default loop
      val defaultLoop = LibUV.uv_default_loop()
      if loop != defaultLoop then
        // Walk all handles and close any that aren't already closing
        LibUV.uv_walk(loop, walkCloseCallback, null.asInstanceOf[Ptr[Byte]])

    /**
     * Close all handles and then close the loop (synchronous).
     *
     * This walks all handles in the loop, closes them, runs the loop
     * until all close callbacks have fired, then closes the loop itself.
     *
     * WARNING: This does blocking uv_run calls which may not be compatible
     * with async effect systems. For cats-effect, prefer using the
     * integrated loop resource which handles cleanup asynchronously.
     *
     * @return Right on success, Left if loop close fails
     */
    def closeDrain: Either[EmileError, Unit] =
      // Check if this is the default loop
      val defaultLoop = LibUV.uv_default_loop()
      if loop == defaultLoop then
        // Don't close the default loop, just return success
        return Right(())

      // Walk all handles and close any that aren't already closing
      // Pass null as arg - use null pointer explicitly
      LibUV.uv_walk(loop, walkCloseCallback, null.asInstanceOf[Ptr[Byte]])

      // Run the loop until all close callbacks have fired
      while LibUV.uv_loop_alive(loop) != 0 do
        val _ = LibUV.uv_run(loop, RunMode.Once.toLibuv)

      // Now close the loop itself
      loop.close

    /**
     * Get the backend file descriptor for embedding the loop.
     *
     * @return The backend fd or -1 if not supported
     */
    def backendFd: Int =
      LibUV.uv_backend_fd(loop)

    /**
     * Get the poll timeout in milliseconds.
     *
     * @return Timeout in ms, or -1 if infinite
     */
    def backendTimeout: Int =
      LibUV.uv_backend_timeout(loop)

    /**
     * Get accumulated idle time in nanoseconds.
     *
     * Requires `LoopConfig.metricsEnabled = true` to be set during loop creation.
     * If metrics were not enabled, this returns 0.
     *
     * @return Accumulated idle time in nanoseconds
     */
    def metricsIdleTime: Long =
      LibUV.uv_metrics_idle_time(loop).toLong

    /** Underlying pointer for advanced usage. */
    inline def ptrUnsafe: Ptr[Byte] = loop

    /** Convert to typed internal pointer. */
    private[emile] inline def toUvLoopPtr: UvLoopPtr = UvLoopPtr(loop)
  end extension

  /** Convert libuv error code to EmileError.SystemError. */
  private[emile] def toSystemError(code: Int): EmileError.SystemError =
    val name = fromCString(LibUV.uv_err_name(code))
    val desc = fromCString(LibUV.uv_strerror(code))
    EmileError.SystemError(ErrorCode(code), s"$name: $desc")

  /** Walk callback that closes each handle if not already closing. */
  private val walkCloseCallback: LibUV.WalkCB = (handle: Ptr[Byte], _: Ptr[Byte]) =>
    if LibUV.uv_is_closing(handle) == 0 then
      LibUV.uv_close(handle, Handle.nullCloseCallback)
end Loop

/**
 * Context function type for operations requiring an event loop.
 */
type LoopContext[A] = Loop ?=> A

/**
 * Context function type for operations requiring an event loop and returning errors.
 */
type LoopResult[A] = Loop ?=> Either[EmileError, A]

object LoopContext:
  /**
   * Execute a block with the default loop in context.
   *
   * @param f The block to execute with loop context
   * @return The result of the block, or error if default loop unavailable
   */
  def withDefault[A](f: Loop ?=> A): Either[EmileError, A] =
    Loop.default.map { loop =>
      given Loop = loop
      f
    }

  /**
   * Execute a block with a new loop in context, closing it afterwards.
   *
   * Uses libuv defaults (no configuration overrides).
   *
   * @param f The block to execute with loop context
   * @return The result of the block, or error if loop creation/close fails
   */
  def withNew[A](f: Loop ?=> A): Either[EmileError, A] =
    Loop.create.flatMap { loop =>
      given Loop = loop
      val result = f
      loop.close.map(_ => result)
    }

  /**
   * Execute a block with a new loop configured with the specified options.
   *
   * @param config Configuration for the loop
   * @param f The block to execute with loop context
   * @return The result of the block, or error if loop creation/close fails
   */
  def withNew[A](config: LoopConfig)(f: Loop ?=> A): Either[EmileError, A] =
    Loop.create(config).flatMap { loop =>
      given Loop = loop
      val result = f
      loop.close.map(_ => result)
    }
end LoopContext
