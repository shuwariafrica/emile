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

import scala.annotation.tailrec
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.netdb
import scala.scalanative.posix.sys.socket
import scala.scalanative.unsafe.CInt
import scala.scalanative.unsafe.CString
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.fromCString
import scala.scalanative.unsafe.sizeof
import scala.scalanative.unsafe.toCString
import scala.scalanative.unsigned.*

import boilerplate.effect.EffIO
import cats.data.NonEmptyList
import cats.effect.IO
import com.comcast.ip4s.Host
import com.comcast.ip4s.Hostname
import com.comcast.ip4s.IDN
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

import emile.unsafe.CallbackBridge
import emile.unsafe.LibUV
import emile.unsafe.LibUVPoller
import emile.unsafe.Routing
import emile.unsafe.SockAddr

/** Forward and reverse name resolution over libuv's asynchronous `getaddrinfo` / `getnameinfo`.
  * Resolution order follows the platform resolver.
  */
object DNS:

  /** Resolves `host` to the socket addresses it serves at `port`. The result is non-empty - libuv
    * reports success only when at least one address is returned.
    */
  def resolve(host: Host, port: Port): EmIO[EmileError.DNS, NonEmptyList[SocketAddress[IpAddress]]] =
    val node = hostString(host)
    lookup(node, port.value.toString, node)

  /** Resolves `host` to its IP addresses. The result is non-empty. */
  def resolve(host: Hostname): EmIO[EmileError.DNS, NonEmptyList[IpAddress]] =
    lookup(host.toString, "0", host.toString).map(_.map(_.host))

  /** Resolves `addr` to a host name via reverse lookup. */
  def reverse(addr: IpAddress): EmIO[EmileError.DNS, Hostname] =
    EffIO
      .liftF(LibUVPollingSystem.currentPoller)
      .flatMap(poller => EffIO.attempt(getNameInfo(poller, addr), EmileError.DNS.Unexpected(_)))

  // IDN must resolve via its ASCII (Punycode) hostname; plain hosts use their text form.
  private def hostString(host: Host): String = host match
    case ip: IpAddress => ip.toString
    case hn: Hostname => hn.toString
    case idn: IDN => idn.hostname.toString

  private def lookup(
    node: String,
    service: String,
    label: String
  ): EmIO[EmileError.DNS, NonEmptyList[SocketAddress[IpAddress]]] =
    EffIO
      .liftF(LibUVPollingSystem.currentPoller)
      .flatMap(poller => EffIO.attempt(getAddrInfo(poller, node, service, label), EmileError.DNS.Unexpected(_)))

  private def getAddrInfo(
    poller: LibUVPoller,
    node: String,
    service: String,
    label: String
  ): IO[NonEmptyList[SocketAddress[IpAddress]]] =
    IO.async[NonEmptyList[SocketAddress[IpAddress]]]: cb =>
      Routing.onOwner(poller):
        val req = allocRequest(LibUV.UV_GETADDRINFO)
        val rc = startGetAddrInfo(poller, req, node, service)
        if rc < 0 then
          stdlib.free(req)
          cb(Left(DNSMapping.fromCode(rc, label)))
          None
        else
          CallbackBridge.storeReq(poller, req, addrDeliver(poller, cb, label))
          // Cancellation cancels the queued getaddrinfo; its callback then fires UV_ECANCELED, which
          // addrDeliver maps to an error and frees the request.
          Some(Routing.onOwner(poller)(LibUV.uv_cancel(req): Unit))

  private def getNameInfo(poller: LibUVPoller, addr: IpAddress): IO[Hostname] =
    IO.async[Hostname]: cb =>
      Routing.onOwner(poller):
        val req = allocRequest(LibUV.UV_GETNAMEINFO)
        val rc = startGetNameInfo(poller, req, addr)
        if rc < 0 then
          stdlib.free(req)
          cb(Left(DNSMapping.fromCode(rc, addr.toString)))
          None
        else
          CallbackBridge.storeReq(poller, req, nameDeliver(poller, cb, addr.toString))
          Some(Routing.onOwner(poller)(LibUV.uv_cancel(req): Unit))

  private def addrDeliver(
    poller: LibUVPoller,
    cb: Either[Throwable, NonEmptyList[SocketAddress[IpAddress]]] => Unit,
    label: String
  ): (Ptr[Byte], Int, Ptr[Byte]) => Unit =
    (req, status, res) =>
      val outcome =
        if status < 0 then Left(DNSMapping.fromCode(status, label))
        else NonEmptyList.fromList(collectAddresses(res)).toRight(EmileError.DNS.UnknownHost(label))
      LibUV.uv_freeaddrinfo(res)
      CallbackBridge.releaseReq(poller, req)
      cb(outcome)

  private def nameDeliver(
    poller: LibUVPoller,
    cb: Either[Throwable, Hostname] => Unit,
    label: String
  ): (Ptr[Byte], Int, CString) => Unit =
    (req, status, hostname) =>
      val outcome: Either[EmileError.DNS, Hostname] =
        if status < 0 then Left(DNSMapping.fromCode(status, label))
        else Hostname.fromString(fromCString(hostname)).toRight(EmileError.DNS.UnknownHost(label))
      CallbackBridge.releaseReq(poller, req)
      cb(outcome)

  // uv_getaddrinfo_cb: run the stored delivery closure, then free the request.
  private val gaiCb: LibUV.GetAddrInfoCB = (req: Ptr[Byte], status: CInt, res: Ptr[Byte]) =>
    val deliver = CallbackBridge.loadReq[(Ptr[Byte], Int, Ptr[Byte]) => Unit](req)
    deliver(req, status, res)
    stdlib.free(req)

  // uv_getnameinfo_cb: the host name points into the request, so run the delivery closure before
  // freeing the request.
  private val niCb: LibUV.GetNameInfoCB = (req: Ptr[Byte], status: CInt, hostname: CString, _: CString) =>
    val deliver = CallbackBridge.loadReq[(Ptr[Byte], Int, CString) => Unit](req)
    deliver(req, status, hostname)
    stdlib.free(req)

  // FFI: request allocation (null calloc result is OOM, surfaced by a throw), the addrinfo
  // hints/result struct reinterpretations, and the null-terminated result-list walk.
  // scalafix:off DisableSyntax
  private def allocRequest(reqType: Int): Ptr[Byte] =
    val req = stdlib.calloc(1.toCSize, LibUV.uv_req_size(reqType))
    if req == null then throw new OutOfMemoryError("emile: libuv request allocation failed")
    else req

  private def startGetAddrInfo(poller: LibUVPoller, req: Ptr[Byte], node: String, service: String): Int =
    val hints = stdlib.calloc(1.toCSize, sizeof[netdb.addrinfo])
    val ai = hints.asInstanceOf[Ptr[netdb.addrinfo]]
    // AI_ADDRCONFIG: return a family's addresses only when the host has an interface configured for
    // it, so an IPv4-only host is not handed AAAA records it cannot reach.
    ai._1 = netdb.AI_ADDRCONFIG
    ai._3 = socket.SOCK_STREAM
    val rc = Zone(LibUV.uv_getaddrinfo(poller.loop, req, gaiCb, toCString(node), toCString(service), hints))
    stdlib.free(hints)
    rc

  private def startGetNameInfo(poller: LibUVPoller, req: Ptr[Byte], addr: IpAddress): Int =
    val storage = stdlib.calloc(1.toCSize, SockAddr.storageSize.toCSize)
    SockAddr.write(SocketAddress(addr, Port.Wildcard), storage)
    // NI_NAMEREQD: fail (EAI_NONAME) when the address has no reverse record, rather than returning
    // its numeric text as though it were a host name.
    val rc = LibUV.uv_getnameinfo(poller.loop, req, niCb, storage, netdb.NI_NAMEREQD)
    stdlib.free(storage)
    rc

  private def collectAddresses(res: Ptr[Byte]): List[SocketAddress[IpAddress]] =
    walkAddrinfo(res.asInstanceOf[Ptr[netdb.addrinfo]], Nil)

  @tailrec
  private def walkAddrinfo(
    node: Ptr[netdb.addrinfo],
    acc: List[SocketAddress[IpAddress]]
  ): List[SocketAddress[IpAddress]] =
    if node == null then acc.reverse
    else
      val entry = SockAddr.read(node._6.asInstanceOf[Ptr[Byte]])
      walkAddrinfo(node._8.asInstanceOf[Ptr[netdb.addrinfo]], entry.fold(acc)(_ :: acc))
  // scalafix:on DisableSyntax

end DNS
