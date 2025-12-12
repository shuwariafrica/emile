/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

import zio.Config
import io.github.arashi01.emile.{EmileConfig, LoopConfig, TcpConfig, TcpKeepAlive}

/**
 * ZIO Config descriptors for Emile configuration types.
 *
 * These top-level givens enable `ZIO.config[LoopConfig]` etc.
 * The default ConfigProvider reads from environment variables and system properties.
 *
 * Usage:
 * {{{
 * import io.github.arashi01.emile.zio.given
 *
 * ZIO.config[LoopConfig]                                // loads from LOOP_* env vars
 * ZIO.config(loopConfig.nested("emile"))                // loads from EMILE_LOOP_*
 * ZIO.config[EmileConfig]                               // loads full config
 * }}}
 */

/**
 * Config descriptor for LoopConfig.
 *
 * Reads from "loop" namespace:
 * - LOOP_METRICS_ENABLED / loop.metricsEnabled
 * - LOOP_BLOCK_SIGNAL / loop.blockSignal
 * - LOOP_USE_IO_URING_SQPOLL / loop.useIoUringSqpoll
 */
given loopConfig: Config[LoopConfig] =
  (
    Config.boolean("metricsEnabled").optional ++
    Config.int("blockSignal").optional ++
    Config.boolean("useIoUringSqpoll").optional
  ).nested("loop").map { case (metrics, signal, sqpoll) =>
    LoopConfig(
      metricsEnabled = metrics,
      blockSignal = signal,
      useIoUringSqpoll = sqpoll
    )
  }

/**
 * Config for TcpKeepAlive ADT.
 *
 * Supports:
 * - delay only -> TcpKeepAlive.Simple(delay)
 * - idle, interval, count -> TcpKeepAlive.Full(...)
 * - neither -> TcpKeepAlive.Disabled
 */
val tcpKeepAliveConfig: Config[TcpKeepAlive] =
  (
    Config.int("idle").optional ++
    Config.int("interval").optional ++
    Config.int("count").optional ++
    Config.int("delay").optional
  ).map { case (idle, interval, count, delay) =>
    (idle, interval, count, delay) match
      case (Some(i), Some(int), Some(c), _) =>
        TcpKeepAlive.Full(i, int, c)
      case (None, None, None, Some(d)) =>
        TcpKeepAlive.Simple(d)
      case _ =>
        TcpKeepAlive.Disabled
  }

/**
 * Config descriptor for TcpConfig.
 *
 * Reads from "tcp" namespace:
 * - TCP_NO_DELAY / tcp.noDelay
 * - TCP_KEEP_ALIVE_* / tcp.keepAlive.*
 * - TCP_SIMULTANEOUS_ACCEPTS / tcp.simultaneousAccepts
 * - TCP_REUSE_PORT / tcp.reusePort
 * - TCP_IPV6_ONLY / tcp.ipv6Only
 */
given tcpConfig: Config[TcpConfig] =
  (
    Config.boolean("noDelay").optional ++
    tcpKeepAliveConfig.optional ++
    Config.boolean("simultaneousAccepts").optional ++
    Config.boolean("reusePort").optional ++
    Config.boolean("ipv6Only").optional
  ).nested("tcp").map { case (noDelay, keepAlive, simAccepts, reusePort, ipv6Only) =>
    TcpConfig(
      noDelay = noDelay,
      keepAlive = keepAlive,
      simultaneousAccepts = simAccepts,
      reusePort = reusePort,
      ipv6Only = ipv6Only
    )
  }

/**
 * Full Emile configuration descriptor.
 *
 * Reads from "emile" namespace:
 * - EMILE_LOOP_* or emile.loop.*
 * - EMILE_TCP_* or emile.tcp.*
 */
given emileConfig: Config[EmileConfig] =
  (loopConfig ++ tcpConfig)
    .nested("emile")
    .map { case (loop, tcp) =>
      EmileConfig(loop, tcp)
    }
