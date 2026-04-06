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

import munit.FunSuite

import emile.ipa.Ipv4Address
import emile.ipa.Ipv6Address
import emile.ipa.Port
import emile.ipa.SocketAddress

/** Tests for EmileConfig, LoopConfig, and TcpConfig.
  *
  * These tests verify:
  *   - Configuration ADT with Option-based overrides
  *   - Separate code paths for default vs configured operations
  *   - Real libuv integration for all configuration paths
  */
class EmileConfigSuite extends FunSuite:
// scalafix:off

  private def expectRight[A](either: Either[?, A]): A =
    either.fold(err => fail(err.toString), identity)

  // ============================================================================
  // EmileConfig ADT Tests
  // ============================================================================

  test("EmileConfig.empty has no overrides"):
    val config = EmileConfig.empty
    assert(!config.hasOverrides)
    assert(!config.hasLoopOverrides)
    assert(!config.hasTcpOverrides)
    assertEquals(config.loop, LoopConfig.empty)
    assertEquals(config.tcp, TcpConfig.empty)

  test("EmileConfig.lowLatency has expected overrides"):
    val config = EmileConfig.lowLatency
    assert(config.hasOverrides)
    assertEquals(config.tcp.noDelay, Some(true))
    assertEquals(config.loop.metricsEnabled, Some(true))

  test("EmileConfig.server has expected overrides"):
    val config = EmileConfig.server
    assert(config.hasOverrides)
    assertEquals(config.tcp.simultaneousAccepts, Some(true))
    config.tcp.keepAlive match
      case Some(TcpKeepAlive.Simple(delay)) =>
        assertEquals(delay, 60, "server keepalive should be 60s")
      case other =>
        fail(s"Expected Some(Simple(60)), got $other")

  test("EmileConfig builder pattern works"):
    val config = EmileConfig.empty
      .withLoop(LoopConfig.empty.withMetricsEnabled(true))
      .withTcp(TcpConfig.empty.withNoDelay(true).withReusePort(true))

    assertEquals(config.loop.metricsEnabled, Some(true))
    assertEquals(config.tcp.noDelay, Some(true))
    assertEquals(config.tcp.reusePort, Some(true))
    // Unspecified values should be None
    assertEquals(config.tcp.keepAlive, None)
    assertEquals(config.tcp.simultaneousAccepts, None)

  // ============================================================================
  // LoopConfig Tests
  // ============================================================================

  test("LoopConfig.empty has all None values"):
    val config = LoopConfig.empty
    assert(!config.hasOverrides)
    assertEquals(config.metricsEnabled, None)
    assertEquals(config.blockSignal, None)
    assertEquals(config.useIoUringSqpoll, None)

  test("LoopConfig builder sets only specified values"):
    val base = LoopConfig.empty
    val withMetrics = base.withMetricsEnabled(true)
    val withSignal = withMetrics.withBlockSignal(27) // SIGPROF

    assert(!base.hasOverrides, "Empty should have no overrides")
    assert(withMetrics.hasOverrides)
    assertEquals(withMetrics.metricsEnabled, Some(true))
    assertEquals(withMetrics.blockSignal, None)
    assertEquals(withSignal.blockSignal, Some(27))

  test("LoopConfig.withMetrics preset"):
    val config = LoopConfig.withMetrics
    assertEquals(config.metricsEnabled, Some(true))
    assertEquals(config.blockSignal, None)
    assertEquals(config.useIoUringSqpoll, None)

  // ============================================================================
  // TcpConfig Tests
  // ============================================================================

  test("TcpConfig.empty has all None values"):
    val config = TcpConfig.empty
    assert(!config.hasOverrides)
    assert(!config.hasHandleOverrides)
    assert(!config.hasBindOverrides)
    assertEquals(config.noDelay, None)
    assertEquals(config.keepAlive, None)
    assertEquals(config.simultaneousAccepts, None)
    assertEquals(config.reusePort, None)
    assertEquals(config.ipv6Only, None)

  test("TcpConfig.lowLatency preset"):
    val config = TcpConfig.lowLatency
    assertEquals(config.noDelay, Some(true))
    assertEquals(config.keepAlive, None) // Not overridden

  test("TcpConfig.server preset"):
    val config = TcpConfig.server
    assertEquals(config.simultaneousAccepts, Some(true))
    assertEquals(config.keepAlive, Some(TcpKeepAlive.Simple(60)))

  test("TcpConfig builder sets only specified values"):
    val config = TcpConfig.empty
      .withNoDelay(true)
      .withKeepAlive(30)
      .withReusePort(true)

    assertEquals(config.noDelay, Some(true))
    assertEquals(config.keepAlive, Some(TcpKeepAlive.Simple(30)))
    assertEquals(config.simultaneousAccepts, None) // Not overridden
    assertEquals(config.reusePort, Some(true))
    assertEquals(config.ipv6Only, None) // Not overridden

  test("TcpConfig.hasHandleOverrides vs hasBindOverrides"):
    val handleOnly = TcpConfig.empty.withNoDelay(true)
    val bindOnly = TcpConfig.empty.withReusePort(true)
    val both = TcpConfig.empty.withNoDelay(true).withIpv6Only(true)

    assert(handleOnly.hasHandleOverrides)
    assert(!handleOnly.hasBindOverrides)

    assert(!bindOnly.hasHandleOverrides)
    assert(bindOnly.hasBindOverrides)

    assert(both.hasHandleOverrides)
    assert(both.hasBindOverrides)

  // ============================================================================
  // TcpKeepAlive Tests
  // ============================================================================

  test("TcpKeepAlive.enabled enforces minimum delay"):
    val keepAlive = TcpKeepAlive.enabled(0)
    keepAlive match
      case TcpKeepAlive.Simple(delay) =>
        assertEquals(delay, 1, "Delay should be at least 1")
      case other =>
        fail(s"Expected Simple, got $other")

  test("TcpKeepAlive.full enforces minimums"):
    val keepAlive = TcpKeepAlive.full(0, -1, 0)
    keepAlive match
      case TcpKeepAlive.Full(idle, interval, count) =>
        assertEquals(idle, 1)
        assertEquals(interval, 1)
        assertEquals(count, 1)
      case other =>
        fail(s"Expected Full, got $other")

  // ============================================================================
  // Loop Configuration Integration Tests (Real libuv)
  // ============================================================================

  test("Loop.create (no config) uses libuv defaults"):
    val result = for
      loop <- Loop.create
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Loop.create with empty config takes fast path"):
    // Empty config should be equivalent to no config
    val result = for
      loop <- Loop.create(LoopConfig.empty)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Loop.create with metrics override"):
    val config = LoopConfig.empty.withMetricsEnabled(true)

    val result = for
      loop <- Loop.create(config)
      idleTime = loop.metricsIdleTime
      _ = assert(idleTime >= 0, s"metricsIdleTime should be >= 0, got $idleTime")
      _ <- loop.close
    yield idleTime

    assert(result.isRight, s"Expected Right, got $result")

  test("Loop.create with io_uring SQPOLL (platform-dependent)"):
    val config = LoopConfig.empty.withIoUringSqpoll(true)

    val result = for
      loop <- Loop.create(config)
      _ <- loop.close
    yield ()

    // Should succeed - ENOSYS is gracefully handled
    assert(result.isRight, s"Expected Right (ENOSYS should be gracefully handled), got $result")

  // ============================================================================
  // TCP Configuration Integration Tests (Real libuv)
  // ============================================================================

  test("Tcp.init (no config) uses libuv defaults"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Tcp.init with empty config takes fast path"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop, TcpConfig.empty)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Tcp.init with overrides applies only specified values"):
    // Only override noDelay - other settings use libuv defaults
    val config = TcpConfig.empty.withNoDelay(true)

    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop, config)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Tcp.init with full TcpConfig.server preset"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop, TcpConfig.server)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Tcp.applyConfig applies only specified overrides"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      // Apply only noDelay override
      _ <- tcp.applyConfig(TcpConfig.empty.withNoDelay(true))
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Tcp.applyConfig with empty config is a no-op"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      // Empty config = no changes
      _ <- tcp.applyConfig(TcpConfig.empty)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  test("Individual TCP setters work"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.setNoDelay(true)
      _ <- tcp.setNoDelay(false)
      _ <- tcp.setKeepAlive(true, 60)
      _ <- tcp.disableKeepAlive
      // Only test disabling — on Windows, re-enabling after disable returns ENOTSUP
      _ <- tcp.setSimultaneousAccepts(false)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Expected Right, got $result")

  // ============================================================================
  // TCP Bind Configuration Tests (Real libuv)
  // ============================================================================

  private def addr(ip: String, port: Int): SocketAddress =
    val ipv4 = expectRight(Ipv4Address.from(ip))
    val p = expectRight(Port.from(port))
    SocketAddress.v4(ipv4, p)

  private def addrV6(ip: String, port: Int): SocketAddress =
    val ipv6 = expectRight(Ipv6Address.from(ip))
    val p = expectRight(Port.from(port))
    SocketAddress.v6(ipv6, p)

  test("Tcp.bind (no config) uses libuv default flags"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.bind(addr("0.0.0.0", 0))
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"bind failed: $result")

  test("Tcp.bind with empty config takes fast path"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.bind(addr("0.0.0.0", 0), TcpConfig.empty)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"bind with empty config failed: $result")

  test("Tcp.bind with ipv6Only override on IPv6 socket"):
    val config = TcpConfig.empty.withIpv6Only(true)

    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.bind(addrV6("::", 0), config)
      sockName <- tcp.getSocketName
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield sockName

    assert(result.isRight, s"bind with ipv6Only failed: $result")

  // ============================================================================
  // LoopContext Tests
  // ============================================================================

  test("LoopContext.withNew uses libuv defaults"):
    val result = LoopContext.withNew { (loop: Loop) ?=>
      loop.isAlive
    }

    assert(result.isRight, s"Expected Right, got $result")

  test("LoopContext.withNew(config) applies overrides"):
    val config = LoopConfig.empty.withMetricsEnabled(true)

    val result = LoopContext.withNew(config) { (loop: Loop) ?=>
      loop.metricsIdleTime
    }

    assert(result.isRight, s"Expected Right, got $result")

  test("LoopContext.withDefault uses global loop"):
    val result = LoopContext.withDefault { (loop: Loop) ?=>
      loop.isAlive
    }

    assert(result.isRight, s"Expected Right, got $result")

  // ============================================================================
  // Full Integration Tests with Presets
  // ============================================================================

  test("Server setup with EmileConfig.server preset"):
    var serverBound = false
    val serverConfig = EmileConfig.server

    val result = for
      loop <- Loop.create(serverConfig.loop)
      server <- Tcp.init(loop, serverConfig.tcp)
      _ <- server.bind(addr("127.0.0.1", 0), serverConfig.tcp)
      sockName <- server.getSocketName
      _ = serverBound = true
      _ = server.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield sockName

    assert(result.isRight, s"Server setup failed: $result")
    assert(serverBound)

  test("Low-latency client with EmileConfig.lowLatency preset"):
    val clientConfig = EmileConfig.lowLatency

    val result = for
      loop <- Loop.create(clientConfig.loop)
      client <- Tcp.init(loop, clientConfig.tcp)
      _ = assertEquals(client.handleType, HandleType.Tcp)
      _ = client.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Client setup failed: $result")

  test("Custom config with keepalive Full"):
    val config = TcpConfig.empty
      .withKeepAlive(Some(TcpKeepAlive.full(10, 5, 3)))

    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop, config)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Keepalive Full config failed: $result")

  test("Mixed config: some overrides, some defaults"):
    // Only override specific values, rest use libuv defaults
    val loopConfig = LoopConfig.empty.withMetricsEnabled(true)
    val tcpConfig = TcpConfig.empty.withNoDelay(true)
    // Note: keepAlive, simultaneousAccepts, reusePort, ipv6Only all use libuv defaults

    val result = for
      loop <- Loop.create(loopConfig)
      tcp <- Tcp.init(loop, tcpConfig)
      _ <- tcp.bind(addr("127.0.0.1", 0))
      idleTime = loop.metricsIdleTime
      _ = assert(idleTime >= 0)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Mixed config failed: $result")
end EmileConfigSuite
