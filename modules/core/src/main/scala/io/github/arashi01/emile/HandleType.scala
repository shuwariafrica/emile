/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

/**
 * Handle type classification matching libuv's handle types.
 *
 * This enum maps to libuv's `uv_handle_type` enum. The `toLibuv` method
 * uses `inline match` for compile-time elimination when the handle type
 * is statically known.
 */
enum HandleType derives CanEqual:
  case Async
  case Check
  case FsEvent
  case FsPoll
  case Handle
  case Idle
  case NamedPipe
  case Poll
  case Prepare
  case Process
  case Stream
  case Tcp
  case Timer
  case Tty
  case Udp
  case Signal
  case Unknown
end HandleType

object HandleType:
  // Inline constants for libuv handle type values
  // These enable compile-time elimination when used with inline match
  private[emile] inline val UV_ASYNC      = 1
  private[emile] inline val UV_CHECK      = 2
  private[emile] inline val UV_FS_EVENT   = 3
  private[emile] inline val UV_FS_POLL    = 4
  private[emile] inline val UV_HANDLE     = 5
  private[emile] inline val UV_IDLE       = 6
  private[emile] inline val UV_NAMED_PIPE = 7
  private[emile] inline val UV_POLL       = 8
  private[emile] inline val UV_PREPARE    = 9
  private[emile] inline val UV_PROCESS    = 10
  private[emile] inline val UV_STREAM     = 11
  private[emile] inline val UV_TCP        = 12
  private[emile] inline val UV_TIMER      = 13
  private[emile] inline val UV_TTY        = 14
  private[emile] inline val UV_UDP        = 15
  private[emile] inline val UV_SIGNAL     = 16
  private[emile] inline val UV_UNKNOWN    = 0

  /**
   * Convert from libuv handle type integer.
   *
   * @param value The libuv UV_*_T constant
   * @return The corresponding HandleType
   */
  private[emile] def fromLibuv(value: Int): HandleType = value match
    case UV_ASYNC      => Async
    case UV_CHECK      => Check
    case UV_FS_EVENT   => FsEvent
    case UV_FS_POLL    => FsPoll
    case UV_HANDLE     => Handle
    case UV_IDLE       => Idle
    case UV_NAMED_PIPE => NamedPipe
    case UV_POLL       => Poll
    case UV_PREPARE    => Prepare
    case UV_PROCESS    => Process
    case UV_STREAM     => Stream
    case UV_TCP        => Tcp
    case UV_TIMER      => Timer
    case UV_TTY        => Tty
    case UV_UDP        => Udp
    case UV_SIGNAL     => Signal
    case _             => Unknown

  /**
   * Convert to libuv handle type integer with compile-time elimination.
   *
   * When called with a statically known HandleType (e.g., `HandleType.Timer.toLibuvInline`),
   * the match is eliminated at compile time and replaced with the literal constant.
   *
   * @param ht The handle type (must be inline for compile-time elimination)
   * @return The libuv UV_* constant value
   */
  private[emile] inline def toLibuvInline(inline ht: HandleType): Int = inline ht match
    case Async     => UV_ASYNC
    case Check     => UV_CHECK
    case FsEvent   => UV_FS_EVENT
    case FsPoll    => UV_FS_POLL
    case Handle    => UV_HANDLE
    case Idle      => UV_IDLE
    case NamedPipe => UV_NAMED_PIPE
    case Poll      => UV_POLL
    case Prepare   => UV_PREPARE
    case Process   => UV_PROCESS
    case Stream    => UV_STREAM
    case Tcp       => UV_TCP
    case Timer     => UV_TIMER
    case Tty       => UV_TTY
    case Udp       => UV_UDP
    case Signal    => UV_SIGNAL
    case Unknown   => UV_UNKNOWN

  extension (ht: HandleType)
    /**
     * Convert to libuv handle type integer for use with uv_handle_size.
     *
     * Based on libuv's uv_handle_type enum in uv.h:
     * UV_UNKNOWN_HANDLE = 0, then UV_HANDLE_TYPE_MAP expands to:
     * UV_ASYNC = 1, UV_CHECK = 2, ..., UV_TIMER = 13, etc.
     *
     * Note: For compile-time elimination when the type is statically known,
     * use `HandleType.toLibuvInline(HandleType.Timer)` instead.
     *
     * @return The libuv UV_* constant value
     */
    private[emile] def toLibuv: Int = ht match
      case Async     => UV_ASYNC
      case Check     => UV_CHECK
      case FsEvent   => UV_FS_EVENT
      case FsPoll    => UV_FS_POLL
      case Handle    => UV_HANDLE
      case Idle      => UV_IDLE
      case NamedPipe => UV_NAMED_PIPE
      case Poll      => UV_POLL
      case Prepare   => UV_PREPARE
      case Process   => UV_PROCESS
      case Stream    => UV_STREAM
      case Tcp       => UV_TCP
      case Timer     => UV_TIMER
      case Tty       => UV_TTY
      case Udp       => UV_UDP
      case Signal    => UV_SIGNAL
      case Unknown   => UV_UNKNOWN
end HandleType
