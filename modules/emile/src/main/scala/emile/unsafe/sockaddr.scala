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
package emile.unsafe

import scala.scalanative.posix.arpa.inet.htons
import scala.scalanative.posix.arpa.inet.ntohs
import scala.scalanative.posix.net.`if`.IF_NAMESIZE
import scala.scalanative.posix.net.`if`.if_indextoname
import scala.scalanative.posix.net.`if`.if_nametoindex
import scala.scalanative.posix.netinet.in.sockaddr_in
import scala.scalanative.posix.netinet.in.sockaddr_in6
import scala.scalanative.posix.sys.socket.AF_INET
import scala.scalanative.posix.sys.socket.AF_INET6
import scala.scalanative.posix.sys.socket.sockaddr_storage
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Ipv6Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress

/** Conversions between ip4s `SocketAddress[IpAddress]` and C `sockaddr` structures, for the libuv
  * socket and DNS calls.
  */
private[emile] object SockAddr:

  /** Bytes to allocate for a `sockaddr` buffer accepting any address family. */
  val storageSize: Int = sizeof[sockaddr_storage].toInt

  /** Writes `address` into `storage` as a `sockaddr_in` or `sockaddr_in6`. `storage` must be at
    * least [[storageSize]] bytes and zeroed; an IPv6 zone id is carried as the numeric interface
    * index in `sin6_scope_id`.
    */
  def write(address: SocketAddress[IpAddress], storage: Ptr[Byte]): Unit =
    val port = htons(address.port.value.toUShort)
    address.host.fold(writeV4(_, storage, port), writeV6(_, storage, port))

  // Ptr[Byte] reinterpreted as a typed sockaddr pointer; scala-native has no typed pointer cast.
  // scalafix:off DisableSyntax

  /** Reads a `SocketAddress` from a `sockaddr_in` / `sockaddr_in6` in `storage`; `None` for any
    * other address family.
    */
  def read(storage: Ptr[Byte]): Option[SocketAddress[IpAddress]] =
    val family = storage.asInstanceOf[Ptr[sockaddr_in]]._1.toInt
    if family == AF_INET then
      val sa = storage.asInstanceOf[Ptr[sockaddr_in]]
      val bytes = new Array[Byte](4)
      copyFromPtr(sa.at3.asInstanceOf[Ptr[Byte]], bytes)
      addressOf(Ipv4Address.fromBytes(bytes), ntohs(sa._2))
    else if family == AF_INET6 then
      val sa = storage.asInstanceOf[Ptr[sockaddr_in6]]
      val bytes = new Array[Byte](16)
      copyFromPtr(sa.at4.asInstanceOf[Ptr[Byte]], bytes)
      val v6 = Ipv6Address.fromBytes(bytes).map(a => scopeName(sa._5).fold(a)(a.withScopeId))
      addressOf(v6, ntohs(sa._2))
    else None

  private def writeV4(v4: Ipv4Address, storage: Ptr[Byte], port: CUnsignedShort): Unit =
    val sa = storage.asInstanceOf[Ptr[sockaddr_in]]
    sa._1 = AF_INET.toUShort
    sa._2 = port
    copyToPtr(v4.toBytes, sa.at3.asInstanceOf[Ptr[Byte]])

  private def writeV6(v6: Ipv6Address, storage: Ptr[Byte], port: CUnsignedShort): Unit =
    val sa = storage.asInstanceOf[Ptr[sockaddr_in6]]
    sa._1 = AF_INET6.toUShort
    sa._2 = port
    copyToPtr(v6.toBytes, sa.at4.asInstanceOf[Ptr[Byte]])
    sa._5 = scopeIndex(v6.scopeId)

  // ip4s carries an IPv6 zone as an interface-name string; the C socket carries it as the numeric
  // interface index in sin6_scope_id (host byte order). A name with no current interface falls back
  // to its numeric form, so a zone is never silently lost on round-trip.

  private def scopeIndex(scopeId: Option[String]): CUnsignedInt =
    scopeId.fold(0.toUInt) { name =>
      val index = Zone(if_nametoindex(toCString(name)))
      if index.toLong != 0L then index else name.toIntOption.fold(0.toUInt)(_.toUInt)
    }

  private def scopeName(index: CUnsignedInt): Option[String] =
    if index.toLong == 0L then None
    else
      val buf = stackalloc[Byte](IF_NAMESIZE)
      val name = if_indextoname(index, buf)
      if name != null then Some(fromCString(name)) else Some(index.toString)

  // scalafix:on DisableSyntax

  private def addressOf(ip: Option[IpAddress], port: CUnsignedShort): Option[SocketAddress[IpAddress]] =
    ip.flatMap(a => Port.fromInt(port.toInt).map(SocketAddress(a, _)))

  private def copyToPtr(src: Array[Byte], dst: Ptr[Byte]): Unit =
    src.indices.foreach(i => dst(i) = src(i))

  private def copyFromPtr(src: Ptr[Byte], dst: Array[Byte]): Unit =
    dst.indices.foreach(i => dst(i) = src(i))

end SockAddr
