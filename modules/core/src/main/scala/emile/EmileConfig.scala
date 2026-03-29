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
package emile

/** Aggregate configuration for the Emile async I/O library.
  *
  * All configuration values are optional - only specified values override the underlying libuv
  * defaults. Instances may be constructed via [[EmileConfig$ EmileConfig]].
  */
final case class EmileConfig(
  loop: LoopConfig,
  tcp: TcpConfig
)

/** Provides factories and extension syntax for [[EmileConfig]]. */
object EmileConfig:
  given CanEqual[EmileConfig, EmileConfig] = CanEqual.derived

  /** Empty configuration - no overrides, uses all libuv defaults. */
  val empty: EmileConfig = EmileConfig(
    loop = LoopConfig.empty,
    tcp = TcpConfig.empty
  )

  /** Configuration optimised for low-latency scenarios. */
  val lowLatency: EmileConfig = EmileConfig(
    loop = LoopConfig.empty.withMetricsEnabled(true),
    tcp = TcpConfig.empty.withNoDelay(true)
  )

  /** Configuration optimised for server workloads. */
  val server: EmileConfig = EmileConfig(
    loop = LoopConfig.empty.withMetricsEnabled(true),
    tcp = TcpConfig.empty
      .withSimultaneousAccepts(true)
      .withKeepAlive(Some(TcpKeepAlive.Simple(60)))
  )

  extension (c: EmileConfig)
    def withLoop(loop: LoopConfig): EmileConfig = c.copy(loop = loop)
    def withTcp(tcp: TcpConfig): EmileConfig = c.copy(tcp = tcp)
    def hasLoopOverrides: Boolean = c.loop.hasOverrides
    def hasTcpOverrides: Boolean = c.tcp.hasOverrides
    def hasOverrides: Boolean = c.hasLoopOverrides || c.hasTcpOverrides
end EmileConfig

/** Configuration overrides for the event loop.
  *
  * All fields are optional - only specified values are applied via `uv_loop_configure`. Unspecified
  * values use libuv's built-in defaults. Instances may be constructed via
  * [[LoopConfig$ LoopConfig]].
  */
final case class LoopConfig(
  metricsEnabled: Option[Boolean],
  blockSignal: Option[Int],
  useIoUringSqpoll: Option[Boolean]
)

/** Provides factories and extension syntax for [[LoopConfig]]. */
object LoopConfig:
  given CanEqual[LoopConfig, LoopConfig] = CanEqual.derived

  /** Empty configuration - no overrides. */
  val empty: LoopConfig = LoopConfig(
    metricsEnabled = None,
    blockSignal = None,
    useIoUringSqpoll = None
  )

  /** Preset: enable metrics collection. */
  val withMetrics: LoopConfig = empty.withMetricsEnabled(true)

  extension (c: LoopConfig)
    def hasOverrides: Boolean =
      c.metricsEnabled.isDefined || c.blockSignal.isDefined || c.useIoUringSqpoll.isDefined

    /** Enable idle time metrics collection. */
    def withMetricsEnabled(enabled: Boolean): LoopConfig =
      c.copy(metricsEnabled = Some(enabled))

    /** Block a signal when polling for new events.
      *
      * Currently only SIGPROF is supported by libuv for suppressing unnecessary wakeups when using
      * a sampling profiler.
      */
    def withBlockSignal(signal: Int): LoopConfig =
      c.copy(blockSignal = Some(signal))

    /** Clear the block signal override. */
    def withoutBlockSignal: LoopConfig =
      c.copy(blockSignal = None)

    /** Enable io_uring SQPOLL mode on Linux for async file operations. */
    def withIoUringSqpoll(enabled: Boolean): LoopConfig =
      c.copy(useIoUringSqpoll = Some(enabled))
  end extension
end LoopConfig

/** Configuration overrides for TCP handles.
  *
  * All fields are optional - only specified values are applied. Unspecified values use libuv's
  * built-in defaults. Instances may be constructed via [[TcpConfig$ TcpConfig]].
  *
  * Settings are categorised by when they are applied:
  *   - Handle options (applied at init): noDelay, keepAlive, simultaneousAccepts
  *   - Bind options (applied at bind): reusePort, ipv6Only
  */
final case class TcpConfig(
  noDelay: Option[Boolean],
  keepAlive: Option[TcpKeepAlive],
  simultaneousAccepts: Option[Boolean],
  reusePort: Option[Boolean],
  ipv6Only: Option[Boolean]
)

/** Provides factories and extension syntax for [[TcpConfig]]. */
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

  /** Preset: enable TCP_NODELAY for low-latency. */
  val lowLatency: TcpConfig = empty.withNoDelay(true)

  /** Preset: server configuration with keepalive. */
  val server: TcpConfig = empty
    .withSimultaneousAccepts(true)
    .withKeepAlive(60)

  extension (c: TcpConfig)
    def hasOverrides: Boolean =
      c.noDelay.isDefined || c.keepAlive.isDefined || c.simultaneousAccepts.isDefined ||
        c.reusePort.isDefined || c.ipv6Only.isDefined

    def hasHandleOverrides: Boolean =
      c.noDelay.isDefined || c.keepAlive.isDefined || c.simultaneousAccepts.isDefined

    def hasBindOverrides: Boolean =
      c.reusePort.isDefined || c.ipv6Only.isDefined

    def withNoDelay(enabled: Boolean): TcpConfig =
      c.copy(noDelay = Some(enabled))

    def withKeepAlive(keepAlive: Option[TcpKeepAlive]): TcpConfig =
      c.copy(keepAlive = keepAlive)

    def withKeepAlive(delaySeconds: Int): TcpConfig =
      c.copy(keepAlive = Some(TcpKeepAlive.Simple(delaySeconds.max(1))))

    def withKeepAliveDisabled: TcpConfig =
      c.copy(keepAlive = Some(TcpKeepAlive.Disabled))

    def withSimultaneousAccepts(enabled: Boolean): TcpConfig =
      c.copy(simultaneousAccepts = Some(enabled))

    def withReusePort(enabled: Boolean): TcpConfig =
      c.copy(reusePort = Some(enabled))

    def withIpv6Only(enabled: Boolean): TcpConfig =
      c.copy(ipv6Only = Some(enabled))
  end extension
end TcpConfig

/** TCP keep-alive configuration.
  *
  * Keep-alive probes detect dead connections by periodically sending packets to verify the peer is
  * still reachable. Instances may be constructed via [[TcpKeepAlive$ TcpKeepAlive]].
  */
enum TcpKeepAlive:
  /** Keep-alive explicitly disabled. */
  case Disabled

  /** Keep-alive enabled with simple configuration.
    *
    * @param delay Initial delay in seconds before sending probes (must be >= 1)
    */
  case Simple(delay: Int)

  /** Keep-alive enabled with full configuration.
    *
    * @param idle Time in seconds connection must be idle before probes start (must be >= 1)
    * @param interval Time in seconds between individual probes (must be >= 1)
    * @param count Number of probes before giving up (must be >= 1)
    */
  case Full(idle: Int, interval: Int, count: Int)
end TcpKeepAlive

/** Provides factories for [[TcpKeepAlive]]. */
object TcpKeepAlive:
  given CanEqual[TcpKeepAlive, TcpKeepAlive] = CanEqual.derived

  def enabled(delaySeconds: Int): TcpKeepAlive =
    Simple(delaySeconds.max(1))

  def full(idleSeconds: Int, intervalSeconds: Int, count: Int): TcpKeepAlive =
    Full(idleSeconds.max(1), intervalSeconds.max(1), count.max(1))
