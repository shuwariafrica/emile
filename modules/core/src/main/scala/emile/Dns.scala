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

// scalafix:off DisableSyntax.null, DisableSyntax.asInstanceOf; Scala Native FFI requires null pointers

import scala.annotation.tailrec
import scala.scalanative.libc.stdlib.calloc
import scala.scalanative.libc.stdlib.free
import scala.scalanative.posix.netdb.addrinfo
import scala.scalanative.posix.sys.socket.sockaddr
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import boilerplate.nullable.*

import emile.ipa.SocketAddress
import emile.ipa.fromSockAddr
import emile.unsafe.CallbackStore
import emile.unsafe.LibUV

/** DNS resolution utilities backed by libuv's uv_getaddrinfo.
  *
  * Unlike libuv handle types, DNS resolution uses a request-based pattern. The request is submitted
  * and a callback is invoked when resolution completes. This avoids blocking the event loop during
  * DNS lookups.
  *
  * ==Usage Example==
  *
  * {{{
  * // Resolve a hostname asynchronously
  * Dns.getAddrInfo(loop, "example.com", "http") { result =>
  *   result match
  *     case Right(addresses) =>
  *       addresses.foreach(addr => println(s"Resolved: ${addr.show}"))
  *     case Left(error) =>
  *       println(s"Resolution failed: $error")
  * }
  *
  * // Run the loop to process the request
  * loop.run(RunMode.Default)
  * }}}
  *
  * ==Thread Safety==
  *
  * DNS resolution callbacks are always invoked from the event loop thread. The underlying libuv
  * implementation may use a thread pool for the actual DNS lookup, but results are delivered via
  * the event loop.
  */
object Dns:
  private val UV_GETADDRINFO = RequestType.GetAddrInfo.toLibuv

  // Null pointer constant for FFI calls (required due to -Yexplicit-nulls)
  private val nullPtr: Ptr[Byte] = null.asInstanceOf[Ptr[Byte]]

  /** Asynchronously resolve a hostname.
    *
    * The callback will be invoked with either an error or a list of resolved socket addresses when
    * the DNS lookup completes.
    *
    * @param loop The event loop
    * @param node The hostname to resolve (e.g., "example.com")
    * @param service The service name or port number (e.g., "http", "80")
    * @param callback Callback invoked with the resolution result
    * @return Either an error (if the request couldn't be initiated) or Unit
    */
  def getAddrInfo(loop: Loop, node: String, service: String)(
    callback: Either[EmileError, List[SocketAddress]] => Unit
  ): Either[EmileError, Unit] =
    val size = LibUV.uv_req_size(UV_GETADDRINFO)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { req =>
      Zone {
        CallbackStore.attachReq(req, callback)

        val nodeC = toCString(node)
        val serviceC = if service.isEmpty then nullPtr.asInstanceOf[CString] else toCString(service)

        val result = LibUV.uv_getaddrinfo(
          loop.ptrUnsafe,
          req,
          getAddrInfoCallback,
          nodeC,
          serviceC,
          nullPtr // No hints - resolve all address families
        )

        if result < 0 then
          // Clean up on failure
          val _ = CallbackStore.detachReq[Either[EmileError, List[SocketAddress]] => Unit](req)
          free(req)
          Left(EmileError.fromErrorCode(ErrorCode(result)))
        else Right(())
      }
    }
  end getAddrInfo

  /** Asynchronously resolve a hostname (service optional).
    *
    * @param loop The event loop
    * @param node The hostname to resolve
    * @param callback Callback invoked with the resolution result
    * @return Either an error or Unit
    */
  def getAddrInfo(loop: Loop, node: String)(
    callback: Either[EmileError, List[SocketAddress]] => Unit
  ): Either[EmileError, Unit] =
    val size = LibUV.uv_req_size(UV_GETADDRINFO)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { req =>
      Zone {
        CallbackStore.attachReq(req, callback)

        val nodeC = toCString(node)

        val result = LibUV.uv_getaddrinfo(
          loop.ptrUnsafe,
          req,
          getAddrInfoCallback,
          nodeC,
          nullPtr.asInstanceOf[CString], // No service
          nullPtr // No hints
        )

        if result < 0 then
          val _ = CallbackStore.detachReq[Either[EmileError, List[SocketAddress]] => Unit](req)
          free(req)
          Left(EmileError.fromErrorCode(ErrorCode(result)))
        else Right(())
      }
    }
  end getAddrInfo

  /** Asynchronously resolve a hostname with hints.
    *
    * @param loop The event loop
    * @param node The hostname to resolve
    * @param service The service name or port number
    * @param family Address family hint (AF_INET, AF_INET6, or AF_UNSPEC)
    * @param socktype Socket type hint (SOCK_STREAM for TCP, SOCK_DGRAM for UDP)
    * @param callback Callback invoked with the resolution result
    * @return Either an error or Unit
    */
  def getAddrInfoWithHints(loop: Loop, node: String, service: String, family: Int, socktype: Int)(
    callback: Either[EmileError, List[SocketAddress]] => Unit
  ): Either[EmileError, Unit] =
    val size = LibUV.uv_req_size(UV_GETADDRINFO)
    calloc(1L, size.toLong).either(EmileError.OutOfMemory).flatMap { req =>
      Zone {
        CallbackStore.attachReq(req, callback)

        val nodeC = toCString(node)
        val serviceC = if service.isEmpty then nullPtr.asInstanceOf[CString] else toCString(service)

        val hints = alloc[addrinfo]()
        hints._1 = 0 // ai_flags
        hints._2 = family // ai_family
        hints._3 = socktype // ai_socktype
        hints._4 = 0 // ai_protocol
        hints._5 = 0.toUInt // ai_addrlen
        hints._6 = nullPtr.asInstanceOf[Ptr[sockaddr]] // ai_addr
        hints._7 = nullPtr.asInstanceOf[CString] // ai_canonname
        hints._8 = nullPtr // ai_next (CVoidPtr)

        val result = LibUV.uv_getaddrinfo(
          loop.ptrUnsafe,
          req,
          getAddrInfoCallback,
          nodeC,
          serviceC,
          hints.asInstanceOf[Ptr[Byte]]
        )

        if result < 0 then
          val _ = CallbackStore.detachReq[Either[EmileError, List[SocketAddress]] => Unit](req)
          free(req)
          Left(EmileError.fromErrorCode(ErrorCode(result)))
        else Right(())
      }
    }
  end getAddrInfoWithHints

  /** Parse the addrinfo linked list into a list of SocketAddresses. */
  private def parseAddrInfo(addrinfo: Ptr[Byte]): List[SocketAddress] =
    @tailrec
    def loop(current: Ptr[addrinfo], acc: List[SocketAddress]): List[SocketAddress] =
      if current == null then acc.reverse
      else
        val sockaddr = current._6 // ai_addr
        val nextAddr =
          if sockaddr != null then
            fromSockAddr(sockaddr) match
              case Right(addr) => addr :: acc
              case Left(_)     => acc // Skip invalid addresses
          else acc
        val next = current._8.asInstanceOf[Ptr[addrinfo]] // ai_next
        loop(next, nextAddr)

    loop(addrinfo.asInstanceOf[Ptr[addrinfo]], Nil)
  end parseAddrInfo

  /** Callback invoked by libuv when DNS resolution completes. */
  private val getAddrInfoCallback: LibUV.GetAddrInfoCB = (req: Ptr[Byte], status: CInt, res: Ptr[Byte]) =>
    CallbackStore.detachReq[Either[EmileError, List[SocketAddress]] => Unit](req).foreach { callback =>
      val result =
        if status < 0 then Left(EmileError.fromErrorCode(ErrorCode(status)))
        else Right(parseAddrInfo(res))

      if res != null then LibUV.uv_freeaddrinfo(res)
      free(req)

      callback(result)
    }

end Dns

// scalafix:on
