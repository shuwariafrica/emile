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

import scala.scalanative.unsafe.*

import boilerplate.effect.EffIO
import cats.effect.IO

import emile.unsafe.LibUV
import emile.unsafe.SockAddr

/** Host network-interface enumeration. */
object Net:

  /** Every address bound to an active network interface (`getifaddrs`), as [[InterfaceAddress]]es -
    * one per bound address, so a dual-stack interface appears several times under one name. Reads
    * host state directly; it needs no libuv loop.
    */
  def interfaces: EmIO[EmileError.IO, List[InterfaceAddress]] =
    EffIO.attempt(IO.delay(enumerate()).flatMap(IO.fromEither), EmileError.IO.Unexpected(_))

  // FFI: the addresses out-parameter, sockaddr-union reinterpretation, and the single-block free.
  // scalafix:off DisableSyntax

  private def enumerate(): Either[EmileError.IO, List[InterfaceAddress]] =
    val addresses = stackalloc[Ptr[LibUV.IfAddr]]()
    val count = stackalloc[CInt]()
    val rc = LibUV.uv_interface_addresses(addresses, count)
    if rc < 0 then Left(IOMapping.fromCode(rc))
    else
      val base = !addresses
      val n = !count
      // Copy every field into owned values before freeing the single block libuv returned.
      val result = (0 until n).iterator.flatMap(i => readInterface(base + i)).toList
      LibUV.uv_free_interface_addresses(base, n)
      Right(result)

  private def readInterface(entry: Ptr[LibUV.IfAddr]): Option[InterfaceAddress] =
    for
      address <- SockAddr.readIp(entry.at5.asInstanceOf[Ptr[Byte]])
      netmask <- SockAddr.readIp(entry.at6.asInstanceOf[Ptr[Byte]])
    yield InterfaceAddress(
      name = fromCString(entry._1),
      mac = macOf(entry.at2.asInstanceOf[Ptr[Byte]]),
      internal = entry._4 != 0,
      address = address,
      netmask = netmask
    )

  // The 6-byte hardware address as colon-separated hex; all-zero (a loopback, say) means none.
  private def macOf(phys: Ptr[Byte]): Option[String] =
    if (0 until 6).forall(i => phys(i) == 0.toByte) then None
    else Some((0 until 6).map(i => f"${phys(i) & 0xff}%02x").mkString(":"))

  // scalafix:on DisableSyntax

end Net
