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

import boilerplate.effect.EffIO
import cats.effect.IO
import com.comcast.ip4s.*

/** Guards the cancellation and cleanup paths on the TCP surface: a cancelable connect bounded by
  * `timeout`, accept cancellation, and server release while a connection arrives.
  */
final class CancellationCleanupSpec extends EmileSuite:

  private val anyLoopback: SocketAddress[IpAddress] =
    SocketAddress(Ipv4Address.fromString("127.0.0.1").get, Port.fromInt(0).get)

  test("a timeout bounds a connect to an unreachable address") {
    // 192.0.2.1 is RFC 5737 TEST-NET-1 (unroutable): a SYN to it draws no reply, so the connect blocks
    // until cancelled. Resource.make's uncancelable acquire could not be aborted by the timeout and
    // this would hang; Resource.makeFull + poll makes the in-flight connect cancelable. The address is
    // unroutable either way, so the attempt always fails - the guard is that it *completes* in bound.
    val unreachable = SocketAddress(ipv4"192.0.2.1", port"9")
    TCP.connect(unreachable).use_.timeout(3.seconds, EmileError.Connect.TimedOut).either.map(_.isLeft).assertEquals(true)
  }

  test("cancelling a waiting accept leaves the listener able to accept") {
    TCP
      .bind(anyLoopback)
      .widen[EmileError]
      .use { server =>
        val acceptOne = server.acceptOne.use_.absolve
        // Hold the connection so the server accepts before the client closes (a rapid close aborts the accept).
        val connectHold =
          TCP.connect(server.address).widen[EmileError].use(_ => EffIO.liftF(IO.sleep(150.millis))).absolve
        EffIO.liftF(acceptOne.timeoutTo(300.millis, IO.unit).flatMap(_ => IO.both(acceptOne, connectHold).map(_ => ())))
      }
      .absolve
      .timeout(5.seconds)
  }

  test("releasing a server while a connection arrives does not crash") {
    // Release the server mid-connection (short window, repeated) to race a connection callback against the close.
    def round: IO[Unit] =
      TCP
        .bind(anyLoopback)
        .widen[EmileError]
        .use { server =>
          val connect = TCP.connect(server.address).widen[EmileError].use_.absolve.attempt.map(_ => ())
          EffIO.liftF(connect.timeoutTo(20.millis, IO.unit))
        }
        .absolve
    round.replicateA_(50).timeout(60.seconds)
  }

end CancellationCleanupSpec
