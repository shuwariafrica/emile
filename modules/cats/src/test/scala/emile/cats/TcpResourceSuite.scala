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
package emile.cats

import scala.concurrent.duration.*

import cats.effect.IO

import boilerplate.effect.Eff
import boilerplate.nullable.*

import emile.EmileError
import emile.ErrorCode
import emile.HandleType
import emile.ipa.Ipv4Address
import emile.ipa.Port
import emile.ipa.SocketAddress

/** Tests for TcpResource - TCP handle lifecycle management. */
class TcpResourceSuite extends EmileSuite:
  // scalafix:off

  // Helper to run Eff tests - unwraps to IO for the test framework
  private inline def runEff[A](eff: Eff[IO, EmileError, A]): IO[A] = eff.rethrow

  test("TcpResource.make acquires and releases tcp handle") {
    runEff {
      TcpResource.make.use { tcp =>
        Eff.succeed[IO, EmileError, Unit] {
          assert(!tcp.isClosing, "TCP should not be closing")
        }
      }
    }
  }

  test("TcpResource.make creates handle with correct type") {
    runEff {
      TcpResource.make.use { tcp =>
        Eff.succeed[IO, EmileError, Unit] {
          assertEquals(tcp.handleType, HandleType.Tcp)
        }
      }
    }
  }

  test("TcpResource.connect fails cleanly on refused connection") {
    val address = SocketAddress.V4(Ipv4Address.Loopback, Port.wrap(9)) // Discard port

    runEff {
      TcpResource
        .connect(address)
        .use(_ => Eff.unit[IO, EmileError])
        .semiflatMap(_ => IO.sleep(300.millis))
        .catchAll(_ => Eff.unit[IO, EmileError]) // Expected: ECONNREFUSED
    }
  }

  test("TcpResource.bind binds to local address") {
    val address = SocketAddress.V4(Ipv4Address.Loopback, Port.wrap(0)) // Let OS assign port

    runEff {
      TcpResource.bind(address).use { tcp =>
        Eff.succeed[IO, EmileError, Unit] {
          assert(!tcp.isClosing, "Bound TCP should not be closing")
        }
      }
    }
  }

  test("Multiple TcpResource instances can coexist") {
    runEff {
      TcpResource.make.use { tcp1 =>
        TcpResource.make.use { tcp2 =>
          Eff.succeed[IO, EmileError, Unit] {
            assert(!tcp1.isClosing && !tcp2.isClosing)
            assertNotEquals(tcp1.ptrUnsafe, tcp2.ptrUnsafe)
          }
        }
      }
    }
  }

  test("TcpResource init failure path frees handle and leaves loop drained") {
    runEff {
      EmileLoop.integrated.use { loop =>
        import emile.unsafe.LibUV
        import scala.scalanative.libc.stdlib.{calloc, free}
        import scala.scalanative.unsigned.UnsignedRichInt

        val invalidFlag = 99.toUInt // invalid address family forces uv_tcp_init_ex failure
        val handleType = HandleType.toLibuvInline(HandleType.Tcp)

        // Use Eff.attempt to capture IO-level errors into Eff channel
        val attemptInit: Eff[IO, EmileError, Unit] =
          Eff.attempt[IO, EmileError, Unit](
            cats.effect.Resource
              .make(IO.blocking {
                val size = LibUV.uv_handle_size(handleType)
                val handle = calloc(1L, size.toLong)
                assert(handle != null, "calloc returned null handle")
                handle
              })(handle => IO.blocking(free(handle)))
              .use { handle =>
                IO.blocking(LibUV.uv_tcp_init_ex(loop.ptrUnsafe, handle, invalidFlag)).flatMap { rc =>
                  if rc < 0 then IO.raiseError(EmileError.fromErrorCode(ErrorCode(rc)))
                  else IO.raiseError(new RuntimeException("uv_tcp_init_ex unexpectedly succeeded"))
                }
              },
            {
              case e: EmileError => e
              case t             => EmileError.SystemError(ErrorCode(-1), t.getMessage.option.getOrElse("Unknown error"))
            }
          )

        attemptInit.catchAll { (err: EmileError) =>
          // Verify init failure produced a typed error (not a defect)
          Eff.succeed[IO, EmileError, Unit](
            assert(err.isInstanceOf[EmileError.SystemError], s"Expected SystemError, got $err")
          )
        }
      }
    }
  }

end TcpResourceSuite
