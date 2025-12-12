/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile

import munit.FunSuite
import io.github.arashi01.emile.ipa.{SocketAddress, Ipv4Address, Ipv6Address, Port}

/**
 * Tests for TCP handle operations.
 *
 * These tests link to and execute the real libuv library, testing:
 * - TCP handle initialization and lifecycle
 * - Server bind/listen/accept
 * - Client connect
 * - Data transfer (read/write)
 * - Socket options (nodelay, keepalive)
 * - Error conditions (bind conflicts, connection refused)
 */
class TcpSuite extends FunSuite:

  // Helper to create SocketAddress.V4 from IP string and port
  private def addr(ip: String, port: Int): SocketAddress =
    val ipv4 = Ipv4Address.parse(ip).toOption.get
    val p = Port.fromInt(port).toOption.get
    SocketAddress.v4(ipv4, p)

  // Helper to extract port from SocketAddress
  private def getPort(sa: SocketAddress): Int = sa match
    case SocketAddress.V4(_, port) => port.value
    case SocketAddress.V6(_, port, _, _) => port.value

  // Helper to extract host string from SocketAddress  
  private def getHost(sa: SocketAddress): String = sa match
    case SocketAddress.V4(ip, _) => ip.show
    case SocketAddress.V6(ip, _, _, _) => ip.show

  test("Tcp.init creates a valid TCP handle"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ = assertEquals(tcp.handleType, HandleType.Tcp)
      _ = assert(!tcp.isActive, "Should not be active initially")
      _ = assert(!tcp.isClosing, "Should not be closing initially")
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield tcp

    assert(result.isRight, s"Expected Right but got $result")

  test("Tcp.bind binds to an address"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      // Bind to port 0 - let OS choose port
      _ <- tcp.bind(addr("0.0.0.0", 0))
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Bind failed: $result")

  test("Tcp.getSocketName returns bound address"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.bind(addr("127.0.0.1", 0))
      sockName <- tcp.getSocketName
      // Should have been assigned a port > 0
      _ = assert(getPort(sockName) > 0, s"Expected ephemeral port > 0, got ${getPort(sockName)}")
      _ = assertEquals(getHost(sockName), "127.0.0.1")
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"getSocketName failed: $result")

  test("Tcp.listen accepts connections"):
    var connectionReceived = false
    var serverRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var loopRef: Loop = null.asInstanceOf[Loop]
    var serverPort: Int = 0

    val result = for
      loop <- Loop.create
      _ = { loopRef = loop }
      server <- Tcp.init(loop)
      _ = { serverRef = server }
      _ <- server.bind(addr("127.0.0.1", 0))
      sockName <- server.getSocketName
      _ = { serverPort = getPort(sockName) }
      _ <- server.listen(128) { status =>
        if status >= 0 then
          connectionReceived = true
          // Accept the connection
          val _ = (for
            acceptedClient <- Tcp.init(loopRef)
            _ <- serverRef.accept(acceptedClient)
          yield { val _ = acceptedClient.close }): Either[EmileError, Unit]
      }
      client <- Tcp.init(loop)
      _ <- client.connect(addr("127.0.0.1", serverPort)) { _ =>
        val _ = client.close
      }
      timer <- Timer.after(loop, Duration.millis(50)) { () =>
        val _ = serverRef.close
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    assert(connectionReceived, "Server should have received a connection")

  test("Tcp.setNoDelay enables TCP_NODELAY"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.setNoDelay(true)
      _ <- tcp.setNoDelay(false)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"setNoDelay failed: $result")

  test("Tcp.setKeepAlive enables SO_KEEPALIVE"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.setKeepAlive(true, 60)
      _ <- tcp.setKeepAlive(false, 0)
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"setKeepAlive failed: $result")

  test("Tcp Handle ref/unref operations work"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ = assert(tcp.hasRef, "Should have ref by default")
      _ = tcp.unref
      _ = assert(!tcp.hasRef, "Should not have ref after unref")
      _ = tcp.ref
      _ = assert(tcp.hasRef, "Should have ref after ref")
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Handle operations failed: $result")

  test("Tcp.listen on already listening port fails with EADDRINUSE"):
    // Note: libuv sets SO_REUSEADDR, so binding to the same port is allowed.
    // The conflict occurs when trying to listen on the same port.
    val result = for
      loop <- Loop.create
      tcp1 <- Tcp.init(loop)
      _ <- tcp1.bind(addr("127.0.0.1", 0))
      sockName <- tcp1.getSocketName
      _ <- tcp1.listen(128)(_ => ())  // First listen succeeds
      tcp2 <- Tcp.init(loop)
      _ <- tcp2.bind(addr("127.0.0.1", getPort(sockName)))  // Bind succeeds due to SO_REUSEADDR
      // Second listen should fail because tcp1 is already listening
      listenResult = tcp2.listen(128)(_ => ())
      _ = assert(listenResult.isLeft, s"Listening on already used port should fail, but got: $listenResult")
      _ = listenResult match
        case Left(EmileError.SystemError(code, _)) =>
          assert(code.value == -98 || code.value == -48, s"Expected EADDRINUSE, got code: ${code.value}")
        case other =>
          fail(s"Expected SystemError, got: $other")
      _ = tcp1.close
      _ = tcp2.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")

  test("Tcp.connect to non-listening port fails with ECONNREFUSED"):
    var connectStatus: Option[Int] = None
    var clientRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]

    val result = for
      loop <- Loop.create
      client <- Tcp.init(loop)
      _ = { clientRef = client }
      _ <- client.connect(addr("127.0.0.1", 59999)) { status =>
        connectStatus = Some(status)
        val _ = clientRef.close
      }
      timer <- Timer.after(loop, Duration.millis(500)) { () =>
        if !clientRef.isClosing then
          val _ = clientRef.close
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    assert(connectStatus.isDefined, "Connect callback should have been called")
    assert(connectStatus.get < 0, s"Connect to non-listening port should fail with negative status: ${connectStatus.get}")

  test("Tcp server and client can communicate"):
    var serverRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var clientRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var loopRef: Loop = null.asInstanceOf[Loop]
    var receivedData: Option[Array[Byte]] = None
    var writeStatus: Option[Int] = None
    var serverPort: Int = 0

    val result = for
      loop <- Loop.create
      _ = { loopRef = loop }
      server <- Tcp.init(loop)
      _ = { serverRef = server }
      _ <- server.bind(addr("127.0.0.1", 0))
      sockName <- server.getSocketName
      _ = { serverPort = getPort(sockName) }
      _ <- server.listen(128) { status =>
        if status >= 0 then
          val _ = (for
            clientHandle <- Tcp.init(loopRef)
            _ <- serverRef.accept(clientHandle)
            _ <- clientHandle.readStart { dataResult =>
              dataResult match
                case Right(data) if data.nonEmpty =>
                  receivedData = Some(data)
                  val _ = clientHandle.readStop
                  val _ = clientHandle.close
                case _ => ()
            }
          yield ()): Either[EmileError, Unit]
      }
      client <- Tcp.init(loop)
      _ = { clientRef = client }
      _ <- client.connect(addr("127.0.0.1", serverPort)) { status =>
        if status >= 0 then
          val data = "Hello, TCP!".getBytes("UTF-8")
          val _ = clientRef.write(data) { wStatus =>
            writeStatus = Some(wStatus)
          }
      }
      timer <- Timer.after(loop, Duration.millis(100)) { () =>
        val _ = clientRef.close
        val _ = serverRef.close
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    assert(receivedData.isDefined, "Server should have received data")
    val received = new String(receivedData.get, "UTF-8")
    assertEquals(received, "Hello, TCP!")
    assert(writeStatus.exists(_ >= 0), s"Write should succeed: $writeStatus")

  test("Tcp.readStop stops reading"):
    var serverRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var clientRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var loopRef: Loop = null.asInstanceOf[Loop]
    var readCount = 0
    var serverPort: Int = 0

    val result = for
      loop <- Loop.create
      _ = { loopRef = loop }
      server <- Tcp.init(loop)
      _ = { serverRef = server }
      _ <- server.bind(addr("127.0.0.1", 0))
      sockName <- server.getSocketName
      _ = { serverPort = getPort(sockName) }
      _ <- server.listen(128) { status =>
        if status >= 0 then
          val _ = (for
            clientHandle <- Tcp.init(loopRef)
            _ <- serverRef.accept(clientHandle)
            _ <- clientHandle.readStart { res =>
              res match
                case Right(data) if data.nonEmpty =>
                  readCount += 1
                  val _ = clientHandle.readStop
                  val _ = clientHandle.close
                case _ => ()
            }
          yield ()): Either[EmileError, Unit]
      }
      client <- Tcp.init(loop)
      _ = { clientRef = client }
      _ <- client.connect(addr("127.0.0.1", serverPort)) { status =>
        if status >= 0 then
          val _ = clientRef.write("First".getBytes("UTF-8"))(_ => ())
          val _ = clientRef.write("Second".getBytes("UTF-8"))(_ => ())
      }
      timer <- Timer.after(loop, Duration.millis(100)) { () =>
        val _ = clientRef.close
        val _ = serverRef.close
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    assert(readCount >= 1, s"Should have read at least once, got $readCount")

  test("Tcp.isReadable and isWritable before connection"):
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ = assert(!tcp.isReadable, "Should not be readable before connection")
      _ = assert(!tcp.isWritable, "Should not be writable before connection")
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")

  test("Tcp.writeString writes string data"):
    var serverRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var clientRef: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var loopRef: Loop = null.asInstanceOf[Loop]
    var receivedData: Option[Array[Byte]] = None
    var serverPort: Int = 0

    val result = for
      loop <- Loop.create
      _ = { loopRef = loop }
      server <- Tcp.init(loop)
      _ = { serverRef = server }
      _ <- server.bind(addr("127.0.0.1", 0))
      sockName <- server.getSocketName
      _ = { serverPort = getPort(sockName) }
      _ <- server.listen(128) { status =>
        if status >= 0 then
          val _ = (for
            clientHandle <- Tcp.init(loopRef)
            _ <- serverRef.accept(clientHandle)
            _ <- clientHandle.readStart { dataResult =>
              dataResult match
                case Right(data) if data.nonEmpty =>
                  receivedData = Some(data)
                  val _ = clientHandle.readStop
                  val _ = clientHandle.close
                case _ => ()
            }
          yield ()): Either[EmileError, Unit]
      }
      client <- Tcp.init(loop)
      _ = { clientRef = client }
      _ <- client.connect(addr("127.0.0.1", serverPort)) { status =>
        if status >= 0 then
          val _ = clientRef.writeString("Hello from writeString!")(_ => ())
      }
      timer <- Timer.after(loop, Duration.millis(100)) { () =>
        val _ = clientRef.close
        val _ = serverRef.close
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    assert(receivedData.isDefined, "Server should have received data")
    val received = new String(receivedData.get, "UTF-8")
    assertEquals(received, "Hello from writeString!")

  test("SocketAddress.v4 creates correct IPv4 address"):
    val sockAddr = addr("0.0.0.0", 8080)
    assertEquals(getHost(sockAddr), "0.0.0.0")
    assertEquals(getPort(sockAddr), 8080)

  test("SocketAddress with loopback creates correct IPv4 address"):
    val sockAddr = addr("127.0.0.1", 3000)
    assertEquals(getHost(sockAddr), "127.0.0.1")
    assertEquals(getPort(sockAddr), 3000)

  test("SocketAddress with IPv6 loopback creates correct address"):
    val ipv6 = Ipv6Address.parse("::1").toOption.get
    val port = Port.fromInt(3000).toOption.get
    val sockAddr = SocketAddress.v6(ipv6, port)
    assertEquals(getHost(sockAddr), "::1")
    assertEquals(getPort(sockAddr), 3000)

  // Callback leak test - ensure restarting listen doesn't leak callbacks
  test("restarting listen does not leak callbacks"):
    val initialSize = unsafe.CallbackRegistry.size
    
    val result = for
      loop <- Loop.create
      tcp <- Tcp.init(loop)
      _ <- tcp.bind(addr("127.0.0.1", 0))
      // First listen
      _ <- tcp.listen(128) { _ => () }
      sizeAfterFirst = unsafe.CallbackRegistry.size
      // Second listen on same handle should replace callback
      _ <- tcp.listen(128) { _ => () }
      sizeAfterSecond = unsafe.CallbackRegistry.size
      // Should not have grown - old callback should be replaced
      _ = assertEquals(sizeAfterSecond, sizeAfterFirst, "Listen callback should be replaced, not added")
      _ = tcp.close
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    // After cleanup, should be back to initial size
    assertEquals(unsafe.CallbackRegistry.size, initialSize)

  // Callback leak test for connect
  test("multiple connects do not leak callbacks"):
    val initialSize = unsafe.CallbackRegistry.size
    var clientRef1: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var clientRef2: Tcp[Open] = null.asInstanceOf[Tcp[Open]]
    var timerRef: Timer[Open] = null.asInstanceOf[Timer[Open]]
    var connectCount = 0

    val result = for
      loop <- Loop.create
      client1 <- Tcp.init(loop)
      _ = { clientRef1 = client1 }
      client2 <- Tcp.init(loop)
      _ = { clientRef2 = client2 }
      // Two separate connects to non-listening ports
      _ <- client1.connect(addr("127.0.0.1", 59997)) { _ =>
        connectCount += 1
        val _ = clientRef1.close
      }
      _ <- client2.connect(addr("127.0.0.1", 59998)) { _ =>
        connectCount += 1
        val _ = clientRef2.close
      }
      timer <- Timer.after(loop, Duration.millis(500)) { () =>
        if !clientRef1.isClosing then { val _ = clientRef1.close }
        if !clientRef2.isClosing then { val _ = clientRef2.close }
        val _ = timerRef.close
      }
      _ = { timerRef = timer }
      _ <- loop.run(RunMode.Default)
      _ <- loop.close
    yield ()

    assert(result.isRight, s"Test failed: $result")
    assertEquals(connectCount, 2, "Both connect callbacks should have been called")
    // Callbacks should be cleaned up after connection completes
    assertEquals(unsafe.CallbackRegistry.size, initialSize)
