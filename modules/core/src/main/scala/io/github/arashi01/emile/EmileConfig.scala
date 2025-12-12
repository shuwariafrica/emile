/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

/**
 * Configuration for the Emile async I/O library.
 *
 * EmileConfig provides a declarative way to configure libuv-based async I/O.
 * All configuration values are optional - only specified values override
 * the underlying libuv defaults. This ensures:
 *
 * - Zero overhead when not customizing (no config passed around)
 * - True override semantics (only explicit changes applied)
 * - Future-proof (libuv default changes don't require emile updates)
 *
 * == Design Philosophy ==
 *
 * Methods come in two variants:
 * - No-config: `Tcp.init(loop)` - uses libuv defaults directly
 * - With-config: `Tcp.init(loop, config)` - applies only specified overrides
 *
 * == Construction ==
 *
 * {{{
 * // Empty config - no overrides
 * val empty = LoopConfig.empty
 *
 * // Override specific values
 * val custom = LoopConfig.empty
 *   .withMetricsEnabled(true)  // Only this is applied
 *
 * // Preset configurations for common use cases
 * val server = EmileConfig.server
 * }}}
 */
final case class EmileConfig(
    loop: LoopConfig,
    tcp: TcpConfig
):
  /** Create a copy with modified loop configuration. */
  def withLoop(loop: LoopConfig): EmileConfig = copy(loop = loop)

  /** Create a copy with modified TCP configuration. */
  def withTcp(tcp: TcpConfig): EmileConfig = copy(tcp = tcp)

  /** Check if any loop configuration overrides are specified. */
  def hasLoopOverrides: Boolean = loop.hasOverrides

  /** Check if any TCP configuration overrides are specified. */
  def hasTcpOverrides: Boolean = tcp.hasOverrides

  /** Check if any configuration overrides are specified. */
  def hasOverrides: Boolean = hasLoopOverrides || hasTcpOverrides

object EmileConfig:
  given CanEqual[EmileConfig, EmileConfig] = CanEqual.derived

  /** Empty configuration - no overrides, uses all libuv defaults. */
  val empty: EmileConfig = EmileConfig(
    loop = LoopConfig.empty,
    tcp = TcpConfig.empty
  )

  /**
   * Configuration optimized for low-latency scenarios.
   *
   * Overrides:
   * - TCP nodelay enabled (disables Nagle's algorithm)
   * - Metrics enabled for monitoring
   */
  val lowLatency: EmileConfig = EmileConfig(
    loop = LoopConfig.empty.withMetricsEnabled(true),
    tcp = TcpConfig.empty.withNoDelay(true)
  )

  /**
   * Configuration optimized for server workloads.
   *
   * Overrides:
   * - Metrics enabled
   * - Simultaneous accepts enabled for better throughput
   * - Keepalive enabled to detect dead connections (60s)
   */
  val server: EmileConfig = EmileConfig(
    loop = LoopConfig.empty.withMetricsEnabled(true),
    tcp = TcpConfig.empty
      .withSimultaneousAccepts(true)
      .withKeepAlive(Some(TcpKeepAlive.Simple(60)))
  )
end EmileConfig

/**
 * Configuration overrides for the event loop.
 *
 * All fields are optional - only specified values are applied via `uv_loop_configure`.
 * Unspecified values use libuv's built-in defaults.
 */
final case class LoopConfig(
    metricsEnabled: Option[Boolean],
    blockSignal: Option[Int],
    useIoUringSqpoll: Option[Boolean]
):
  /** Check if any overrides are specified. */
  def hasOverrides: Boolean =
    metricsEnabled.isDefined || blockSignal.isDefined || useIoUringSqpoll.isDefined

  /** Enable idle time metrics collection. */
  def withMetricsEnabled(enabled: Boolean): LoopConfig =
    copy(metricsEnabled = Some(enabled))

  /**
   * Block a signal when polling for new events.
   *
   * Currently only SIGPROF is supported by libuv for suppressing
   * unnecessary wakeups when using a sampling profiler.
   */
  def withBlockSignal(signal: Int): LoopConfig =
    copy(blockSignal = Some(signal))

  /** Clear the block signal override. */
  def withoutBlockSignal: LoopConfig =
    copy(blockSignal = None)

  /**
   * Enable io_uring SQPOLL mode on Linux for async file operations.
   *
   * Note: Only available on Linux with io_uring support.
   */
  def withIoUringSqpoll(enabled: Boolean): LoopConfig =
    copy(useIoUringSqpoll = Some(enabled))

object LoopConfig:
  given CanEqual[LoopConfig, LoopConfig] = CanEqual.derived

  /** Empty configuration - no overrides. */
  val empty: LoopConfig = LoopConfig(
    metricsEnabled = None,
    blockSignal = None,
    useIoUringSqpoll = None
  )

  /** Preset: Enable metrics collection. */
  val withMetrics: LoopConfig = empty.withMetricsEnabled(true)
end LoopConfig

/**
 * Configuration overrides for TCP handles.
 *
 * All fields are optional - only specified values are applied.
 * Unspecified values use libuv's built-in defaults.
 *
 * Settings are categorized by when they are applied:
 * - Handle options (applied at init): noDelay, keepAlive, simultaneousAccepts
 * - Bind options (applied at bind): reusePort, ipv6Only
 */
final case class TcpConfig(
    noDelay: Option[Boolean],
    keepAlive: Option[TcpKeepAlive],
    simultaneousAccepts: Option[Boolean],
    reusePort: Option[Boolean],
    ipv6Only: Option[Boolean]
):
  /** Check if any overrides are specified. */
  def hasOverrides: Boolean =
    noDelay.isDefined || keepAlive.isDefined || simultaneousAccepts.isDefined ||
      reusePort.isDefined || ipv6Only.isDefined

  /** Check if any handle-time overrides are specified. */
  def hasHandleOverrides: Boolean =
    noDelay.isDefined || keepAlive.isDefined || simultaneousAccepts.isDefined

  /** Check if any bind-time overrides are specified. */
  def hasBindOverrides: Boolean =
    reusePort.isDefined || ipv6Only.isDefined

  /** Enable or disable TCP_NODELAY (Nagle's algorithm). */
  def withNoDelay(enabled: Boolean): TcpConfig =
    copy(noDelay = Some(enabled))

  /** Configure TCP keep-alive settings. */
  def withKeepAlive(keepAlive: Option[TcpKeepAlive]): TcpConfig =
    copy(keepAlive = keepAlive)

  /** Enable keep-alive with the specified delay. */
  def withKeepAlive(delaySeconds: Int): TcpConfig =
    copy(keepAlive = Some(TcpKeepAlive.Simple(delaySeconds.max(1))))

  /** Disable keep-alive explicitly. */
  def withKeepAliveDisabled: TcpConfig =
    copy(keepAlive = Some(TcpKeepAlive.Disabled))

  /**
   * Enable or disable simultaneous accept requests.
   *
   * When enabled, the OS queues multiple accept requests which improves
   * connection acceptance rate but may lead to uneven load distribution
   * in multi-process setups.
   */
  def withSimultaneousAccepts(enabled: Boolean): TcpConfig =
    copy(simultaneousAccepts = Some(enabled))

  /**
   * Enable or disable SO_REUSEPORT.
   *
   * When enabled, allows multiple processes/threads to bind to the same
   * port for load balancing. Only available on Linux 3.9+, DragonFlyBSD 3.6+,
   * FreeBSD 12.0+, Solaris 11.4, and AIX 7.2.5+.
   */
  def withReusePort(enabled: Boolean): TcpConfig =
    copy(reusePort = Some(enabled))

  /**
   * Enable or disable IPv6-only mode.
   *
   * When enabled on an IPv6 socket, disables dual-stack support.
   */
  def withIpv6Only(enabled: Boolean): TcpConfig =
    copy(ipv6Only = Some(enabled))

object TcpConfig:
  given CanEqual[TcpConfig, TcpConfig] = CanEqual.derived

  /** Empty configuration - no overrides. */
  val empty: TcpConfig = TcpConfig(
    noDelay = None,
    keepAlive = None,
    simultaneousAccepts = None,
    reusePort = None,
    ipv6Only = None
  )

  /** Preset: Enable TCP_NODELAY for low-latency. */
  val lowLatency: TcpConfig = empty.withNoDelay(true)

  /** Preset: Server configuration with keepalive. */
  val server: TcpConfig = empty
    .withSimultaneousAccepts(true)
    .withKeepAlive(60)
end TcpConfig

/**
 * TCP keep-alive configuration.
 *
 * Keep-alive probes detect dead connections by periodically sending
 * packets to verify the peer is still reachable.
 */
enum TcpKeepAlive:
  /** Keep-alive explicitly disabled. */
  case Disabled

  /**
   * Keep-alive enabled with simple configuration.
   *
   * @param delay Initial delay in seconds before sending probes (must be >= 1)
   */
  case Simple(delay: Int)

  /**
   * Keep-alive enabled with full configuration.
   *
   * @param idle Time in seconds connection must be idle before probes start (must be >= 1)
   * @param interval Time in seconds between individual probes (must be >= 1)
   * @param count Number of probes before giving up (must be >= 1)
   */
  case Full(idle: Int, interval: Int, count: Int)

object TcpKeepAlive:
  given CanEqual[TcpKeepAlive, TcpKeepAlive] = CanEqual.derived

  /**
   * Create a simple keep-alive configuration.
   *
   * @param delaySeconds Initial delay before probes (must be >= 1)
   */
  def enabled(delaySeconds: Int): TcpKeepAlive =
    Simple(delaySeconds.max(1))

  /**
   * Create a full keep-alive configuration.
   *
   * @param idleSeconds Idle time before probes start (must be >= 1)
   * @param intervalSeconds Time between probes (must be >= 1)
   * @param count Number of probes (must be >= 1)
   */
  def full(idleSeconds: Int, intervalSeconds: Int, count: Int): TcpKeepAlive =
    Full(idleSeconds.max(1), intervalSeconds.max(1), count.max(1))
end TcpKeepAlive
