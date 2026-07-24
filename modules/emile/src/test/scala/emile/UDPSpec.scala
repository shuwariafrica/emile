/*
 * Copyright 2025, 2026 Ali Rashid
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

import scala.concurrent.duration.*

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// Covers the portable UDPSocket baseline (UDP.bind): bound and connected round-trips, the empty
// datagram sentinel, the batched trySend, multicast membership, and a reusePort bind, over loopback.
final class UDPSpec extends EmileSuite:

  private val loopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")

  test("bind on loopback yields a loopback address with a kernel-picked port") {
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(socket =>
        EffIO.suspend:
          assert(socket.localAddress.host.isLoopback)
          assert(socket.localAddress.port.value != 0)
      )
      .absolve
      .timeout(5.seconds)
  }

  test("bound datagram round-trip delivers the payload and the sender's address") {
    val msg = bytes("hello-udp-baseline")
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .bind(loopback)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- sender.send(Slice.of(msg), receiver.localAddress).absolve
                got <- receiver.receive.take(1).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assertEquals(got.head.payload.toList, msg.toList))
                _ <- IO(assertEquals(got.head.peer.port, sender.localAddress.port))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("connected datagram round-trip via the address-free send") {
    val msg = bytes("connected-udp")
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .bind(loopback)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- sender.connect(receiver.localAddress).absolve
                peer <- sender.peerAddress.absolve
                _ <- IO(assertEquals(peer.port, receiver.localAddress.port))
                _ <- sender.send(Slice.of(msg)).absolve
                got <- receiver.receive.take(1).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assertEquals(got.head.payload.toList, msg.toList))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("an empty datagram delivers an empty payload with its peer") {
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .bind(loopback)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- sender.send(Slice.empty, receiver.localAddress).absolve
                got <- receiver.receive.take(1).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assert(got.head.payload.isEmpty))
                _ <- IO(assertEquals(got.head.peer.port, sender.localAddress.port))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("batched trySend accepts the whole batch and every datagram arrives") {
    val batchSize = 5
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .bind(loopback)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                datagrams <- IO(
                               (0 until batchSize).toList.map(i => (Slice.of(bytes(s"batch-$i")), receiver.localAddress))
                             )
                sent <- sender.trySend(datagrams).absolve
                _ <- IO(assertEquals(sent, batchSize))
                got <- receiver.receive.take(batchSize.toLong).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assertEquals(got.size, batchSize))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("a single trySend of a valid datagram is accepted") {
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .bind(loopback)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                accepted <- sender.trySend(Slice.of(bytes("try")), receiver.localAddress).absolve
                _ <- IO(assert(accepted))
              yield ()
            )
          )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("multicast join and leave succeed on the default interface") {
    val group = Ipv4Address.fromString("239.255.0.1").get
    UDP
      .bind(loopback)
      .widen[EmileError]
      .use(socket =>
        EffIO.liftF(
          for
            _ <- socket.join(group, None).absolve
            _ <- socket.leave(group, None).absolve
          yield ()
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("two sockets bind the same port with reusePort") {
    UDP
      .bind(loopback, UDPOptions.default.copy(reusePort = true))
      .widen[EmileError]
      .use(first =>
        UDP
          .bind(SocketAddress(first.localAddress.host, first.localAddress.port), UDPOptions.default.copy(reusePort = true))
          .widen[EmileError]
          .use(second => EffIO.suspend(assertEquals(second.localAddress.port, first.localAddress.port)))
      )
      .absolve
      .timeout(5.seconds)
  }

end UDPSpec
