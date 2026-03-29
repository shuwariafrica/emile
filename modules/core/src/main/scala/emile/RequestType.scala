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

import scala.scalanative.unsafe.CInt

/** Enumeration of libuv request types.
  *
  * This provides a type-safe way to specify request types for libuv operations. The values are
  * derived from libuv's `uv_req_type` enum in `uv.h`:
  *
  * {{{
  * typedef enum {
  *   UV_UNKNOWN_REQ = 0,
  *   UV_REQ,
  *   UV_CONNECT,
  *   UV_WRITE,
  *   UV_SHUTDOWN,
  *   UV_UDP_SEND,
  *   UV_FS,
  *   UV_WORK,
  *   UV_GETADDRINFO,
  *   UV_GETNAMEINFO,
  *   UV_REQ_TYPE_MAX,
  * } uv_req_type;
  * }}}
  */
enum RequestType derives CanEqual:
  case Unknown // 0
  case Req // 1
  case Connect // 2
  case Write // 3
  case Shutdown // 4
  case UdpSend // 5
  case Fs // 6
  case Work // 7
  case GetAddrInfo // 8
  case GetNameInfo // 9

object RequestType:
  /** Convert a libuv request type constant to a RequestType.
    *
    * @param value The libuv CInt value
    * @return The corresponding RequestType, or Unknown if not recognized
    */
  def fromLibuv(value: CInt): RequestType =
    value match
      case 0 => Unknown
      case 1 => Req
      case 2 => Connect
      case 3 => Write
      case 4 => Shutdown
      case 5 => UdpSend
      case 6 => Fs
      case 7 => Work
      case 8 => GetAddrInfo
      case 9 => GetNameInfo
      case _ => Unknown

  /** Extension method to convert RequestType to libuv CInt constant.
    *
    * This is the safe way to get the correct constant value for libuv calls.
    */
  extension (rt: RequestType)
    def toLibuv: CInt = rt match
      case Unknown     => 0
      case Req         => 1
      case Connect     => 2
      case Write       => 3
      case Shutdown    => 4
      case UdpSend     => 5
      case Fs          => 6
      case Work        => 7
      case GetAddrInfo => 8
      case GetNameInfo => 9
  end extension
end RequestType
