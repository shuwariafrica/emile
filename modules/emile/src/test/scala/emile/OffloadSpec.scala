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

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

import boilerplate.effect.EffIO
import cats.effect.IO
import fs2.Chunk
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

// The offload combinator: lane execution and hand-back, an offloaded socket round-trip, a connect
// scope run offloaded, and the infallible channel.
final class OffloadSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("offload runs on an emile-offload lane thread and returns to the runtime") {
    val laneThread = new AtomicReference[String]("")
    val afterThread = new AtomicReference[String]("")
    val program =
      for
        _ <- EffIO.suspend(laneThread.set(Thread.currentThread().getName)).offload
        _ <- EffIO.suspend(afterThread.set(Thread.currentThread().getName))
      yield ()
    program.absolve.timeout(5.seconds).map { _ =>
      assert(laneThread.get.startsWith("emile-offload-"), s"lane thread was ${laneThread.get}")
      assert(!afterThread.get.startsWith("emile-offload-"), s"continuation stayed on ${afterThread.get}")
    }
  }

  test("a socket write and read driven offloaded round-trips") {
    val payload: Chunk[Byte] = Chunk.array("offloaded".getBytes("UTF-8"))
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use { server =>
        EffIO.liftF {
          val srv =
            server.accepted
              .evalMap(
                _.use(s =>
                  s.read(payload.size).flatMap {
                    case Some(chunk) => s.write(chunk)
                    case None => EffIO.succeed(())
                  }
                )
              )
              .head
              .compile
              .drain
              .absolve
          val cli =
            TCP
              .connect(server.address)
              .widen[EmileError]
              .use(s =>
                EffIO.liftF(
                  // The write and read register and hand back to the owner loop from a lane thread.
                  (s.write(payload) *> s.read(payload.size)).offload.absolve
                    .map(_.fold("")(c => new String(c.toArray, "UTF-8")))
                )
              )
              .absolve
          IO.both(srv, cli).map(_._2)
        }
      }
      .absolve
      .map(result => assertEquals(result, "offloaded"))
      .timeout(10.seconds)
  }

  test("a whole TCP.connect scope runs offloaded") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use { server =>
        EffIO.liftF {
          val srv = server.accepted.evalMap(_.use(_ => EffIO.liftF(IO.sleep(50.millis)))).head.compile.drain.absolve
          val conn = TCP.connect(server.address).widen[EmileError].use(_ => EffIO.succeed(true)).offload.absolve
          IO.both(srv, conn).map(_._2)
        }
      }
      .absolve
      .map(ok => assert(ok))
      .timeout(10.seconds)
  }

  test("offload keeps E = Nothing on an infallible effect and lets a defect pass through") {
    val defect = new RuntimeException("boom")
    val offloaded: EmIO[Nothing, Int] = EffIO.succeed(21).offload
    val defective: EmIO[Nothing, Int] = EffIO.liftF(IO.raiseError[Int](defect)).offload
    for
      value <- offloaded.absolve
      caught <- defective.absolve.attempt
    yield
      assertEquals(value, 21)
      caught match
        case Left(t) => assertEquals(t.getMessage, "boom")
        case Right(v) => fail(s"expected the defect to propagate, got: $v")
  }

end OffloadSpec
