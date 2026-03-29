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

/** Re-exports from emile-core and emile-ipa for emile-cats users.
  *
  * {{{
  * import emile.cats.*
  * }}}
  */

// Core Handle Types and Companions
export emile.{Loop, Tcp, Timer, Async, Poll, Signal, Dns}

// Handle State Types
export emile.{HandleState, Open, Closed}

// Configuration Types
export emile.{LoopConfig, TcpConfig, TcpKeepAlive, Timeout}

// Error Types
export emile.{EmileError, ErrorCode}

// IPA Types (Addresses)
export emile.ipa.{SocketAddress, Ipv4Address, Ipv6Address, Port, AddressError}

/** Literal interpolators for compile-time validated addresses and ports. */
val literals: emile.ipa.literals.type = emile.ipa.literals
