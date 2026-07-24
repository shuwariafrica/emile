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
import scala.scalanative.posix.errno as posixErrno

import boilerplate.Slice
import boilerplate.effect.EffIO
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Ipv6Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// Covers the Linux batched UDPChannel (UDP.channel): per-packet ECN both families, PKTINFO local /
// interface delivery, GSO send segmentation, transparent GRO receive splitting, MessageTooLarge on an
// oversized DF send, pathMtu, and runtime capability detection, over loopback.
final class UDPChannelSpec extends EmileSuite:

  private def v4: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  private def v6: SocketAddress[IpAddress] =
    SocketAddress(Ipv6Address.fromString("::1").get, Port.fromInt(0).get)

  private def bytes(n: Int): Array[Byte] = Array.fill(n)(0x61.toByte)

  private def ecnRoundTrip(address: SocketAddress[IpAddress]): IO[Unit] =
    UDP
      .channel(address, ChannelConfig.default)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .channel(address, ChannelConfig.default)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              ECN.values.toList.traverse_ { cp =>
                for
                  _ <- sender.sendBatch(List(Outbound(Slice.of(bytes(4)), receiver.localAddress, cp, None, None))).absolve
                  got <- receiver.receive.take(1).compile.toList.absolve.timeout(5.seconds)
                  _ <- IO(assertEquals(got.head.ecn, cp))
                yield ()
              }
            )
          )
      )
      .absolve

  test("per-packet ECN round-trips all four codepoints over IPv4") {
    ecnRoundTrip(v4).timeout(20.seconds)
  }

  test("per-packet ECN round-trips all four codepoints over IPv6") {
    ecnRoundTrip(v6).timeout(20.seconds)
  }

  test("PKTINFO reports the local address and arrival interface per datagram") {
    UDP
      .channel(v4, ChannelConfig.default.copy(pktinfo = true))
      .widen[EmileError]
      .use(receiver =>
        UDP
          .channel(v4, ChannelConfig.default)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- sender.sendBatch(List(Outbound(Slice.of(bytes(8)), receiver.localAddress, ECN.NotEct, None, None))).absolve
                got <- receiver.receive.take(1).compile.toList.absolve.timeout(5.seconds)
                local <- IO.fromOption(got.head.local)(new AssertionError("expected PKTINFO local info"))
                _ <- IO(assert(local.interfaceIndex > 0))
                _ <- IO(assert(local.address.isLoopback))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("GSO segments one send into N datagrams a non-GRO receiver sees individually") {
    val segment = 1000
    val payload = bytes(4 * segment)
    UDP
      .channel(v4, ChannelConfig.default)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .channel(v4, ChannelConfig.default)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- IO(assert(sender.capabilities.gso))
                _ <- sender
                       .sendBatch(List(Outbound(Slice.of(payload), receiver.localAddress, ECN.NotEct, None, Some(segment))))
                       .absolve
                got <- receiver.receive.take(4).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assertEquals(got.size, 4))
                _ <- IO(assert(got.forall(_.payload.length == segment)))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("GRO coalesces on receive and the channel splits it back into per-segment datagrams") {
    val segment = 1000
    val payload = bytes(4 * segment)
    UDP
      .channel(v4, ChannelConfig.default.copy(gro = true))
      .widen[EmileError]
      .use(receiver =>
        UDP
          .channel(v4, ChannelConfig.default)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- IO(assert(receiver.capabilities.gro))
                _ <- sender
                       .sendBatch(List(Outbound(Slice.of(payload), receiver.localAddress, ECN.NotEct, None, Some(segment))))
                       .absolve
                got <- receiver.receive.take(4).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assertEquals(got.size, 4))
                _ <- IO(assert(got.forall(_.payload.length == segment)))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("an oversized send under Don't-Fragment fails with MessageTooLarge") {
    UDP
      .channel(v4, ChannelConfig.default.copy(pmtud = PmtudMode.Do))
      .widen[EmileError]
      .use(sender =>
        UDP
          .channel(v4, ChannelConfig.default)
          .widen[EmileError]
          .use(receiver =>
            EffIO.liftF(
              for
                result <- sender.sendBatch(List(Outbound(Slice.of(bytes(70000)), receiver.localAddress, ECN.NotEct, None, None))).either
                _ <- IO(assertEquals(result, Left(EmileError.IO.MessageTooLarge)))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("capability detection reports GSO, GRO, and a measured maximum segment count") {
    UDP
      .channel(v4, ChannelConfig.default)
      .widen[EmileError]
      .use(channel =>
        EffIO.suspend:
          val caps = channel.capabilities
          assert(caps.gso)
          assert(caps.gro)
          assert(caps.maxGsoSegments >= 64)
      )
      .absolve
      .timeout(5.seconds)
  }

  test("pathMtu on an unconnected channel is a typed ENOTCONN error") {
    UDP
      .channel(v4, ChannelConfig.default.copy(pmtud = PmtudMode.Do))
      .widen[EmileError]
      .use(channel =>
        EffIO.liftF(
          channel.pathMtu.either.flatMap {
            case Left(EmileError.IO.System(code)) => IO(assertEquals(code, ErrorCode(-posixErrno.ENOTCONN)))
            case other => IO(fail(s"expected a typed ENOTCONN System error, got $other"))
          }
        )
      )
      .absolve
      .timeout(5.seconds)
  }

  test("connect makes pathMtu readable and reports the peer, and disconnect reverts it") {
    UDP
      .channel(v4, ChannelConfig.default.copy(pmtud = PmtudMode.Do))
      .widen[EmileError]
      .use(receiver =>
        UDP
          .channel(v4, ChannelConfig.default.copy(pmtud = PmtudMode.Do))
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- sender.connect(receiver.localAddress).absolve
                peer <- sender.peerAddress.absolve
                _ <- IO(assertEquals(peer.port, receiver.localAddress.port))
                mtu <- sender.pathMtu.absolve
                _ <- IO(assert(mtu > 0))
                _ <- sender.disconnect.absolve
                afterDisconnect <- sender.pathMtu.either
                _ <- IO(assert(afterDisconnect.isLeft))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

  test("a connected channel round-trips a datagram to its peer") {
    val msg = bytes(24)
    UDP
      .channel(v4, ChannelConfig.default)
      .widen[EmileError]
      .use(receiver =>
        UDP
          .channel(v4, ChannelConfig.default)
          .widen[EmileError]
          .use(sender =>
            EffIO.liftF(
              for
                _ <- sender.connect(receiver.localAddress).absolve
                _ <- sender
                       .sendBatch(List(Outbound(Slice.of(msg), receiver.localAddress, ECN.NotEct, None, None)))
                       .absolve
                got <- receiver.receive.take(1).compile.toList.absolve.timeout(5.seconds)
                _ <- IO(assertEquals(got.head.payload.length, msg.length))
                _ <- IO(assertEquals(got.head.peer.port, sender.localAddress.port))
              yield ()
            )
          )
      )
      .absolve
      .timeout(10.seconds)
  }

end UDPChannelSpec
