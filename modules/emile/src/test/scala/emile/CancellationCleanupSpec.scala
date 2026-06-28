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

import com.comcast.ip4s.*

/** Guards the cancelable connect: a `timeout` bounds a connect to an unreachable peer instead of
  * hanging on an uncancelable acquire.
  */
final class CancellationCleanupSpec extends EmileSuite:

  test("a timeout bounds a connect to an unreachable address") {
    // 192.0.2.1 is RFC 5737 TEST-NET-1 (unroutable): a SYN to it draws no reply, so the connect blocks
    // until cancelled. Resource.make's uncancelable acquire could not be aborted by the timeout and
    // this would hang; Resource.makeFull + poll makes the in-flight connect cancelable. The address is
    // unroutable either way, so the attempt always fails - the guard is that it *completes* in bound.
    val unreachable = SocketAddress(ipv4"192.0.2.1", port"9")
    Tcp.connect(unreachable).use_.timeout(3.seconds, EmileError.Connect.TimedOut).either.map(_.isLeft).assertEquals(true)
  }
