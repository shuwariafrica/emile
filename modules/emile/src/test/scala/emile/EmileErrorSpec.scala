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

/** Covers the [[EmileError]] vocabulary: the four code-dispatch tables, the collapsing `Unexpected`
  * constructors, [[SignalNumber]] validation, and the [[LoopConfig]] presets.
  */
final class EmileErrorSpec extends munit.FunSuite:

  test("BindMapping dispatches each known code and falls through to System") {
    assertEquals(BindMapping.fromCode(ErrorCode.UV_EADDRINUSE), EmileError.Bind.AddressInUse: EmileError.Bind)
    assertEquals(BindMapping.fromCode(ErrorCode.UV_EADDRNOTAVAIL), EmileError.Bind.AddressNotAvailable: EmileError.Bind)
    assertEquals(BindMapping.fromCode(ErrorCode.UV_EACCES), EmileError.Bind.PermissionDenied: EmileError.Bind)
    assertEquals(BindMapping.fromCode(-9999), EmileError.Bind.System(ErrorCode(-9999)): EmileError.Bind)
  }

  test("ConnectMapping dispatches each known code and falls through to System") {
    assertEquals(ConnectMapping.fromCode(ErrorCode.UV_ECONNREFUSED), EmileError.Connect.ConnectionRefused: EmileError.Connect)
    assertEquals(ConnectMapping.fromCode(ErrorCode.UV_ENETUNREACH), EmileError.Connect.NetworkUnreachable: EmileError.Connect)
    assertEquals(ConnectMapping.fromCode(ErrorCode.UV_EHOSTUNREACH), EmileError.Connect.HostUnreachable: EmileError.Connect)
    assertEquals(ConnectMapping.fromCode(ErrorCode.UV_ETIMEDOUT), EmileError.Connect.TimedOut: EmileError.Connect)
    assertEquals(ConnectMapping.fromCode(-9999), EmileError.Connect.System(ErrorCode(-9999)): EmileError.Connect)
  }

  test("IOMapping dispatches each known code and falls through to System") {
    assertEquals(IOMapping.fromCode(ErrorCode.UV_EOF), EmileError.IO.EndOfStream: EmileError.IO)
    assertEquals(IOMapping.fromCode(ErrorCode.UV_ECONNRESET), EmileError.IO.ConnectionReset: EmileError.IO)
    assertEquals(IOMapping.fromCode(ErrorCode.UV_EPIPE), EmileError.IO.BrokenPipe: EmileError.IO)
    assertEquals(IOMapping.fromCode(-9999), EmileError.IO.System(ErrorCode(-9999)): EmileError.IO)
  }

  test("DNSMapping maps the name-resolution codes to UnknownHost") {
    assertEquals(DNSMapping.fromCode(-3008, "host.example"), EmileError.DNS.UnknownHost("host.example"): EmileError.DNS)
    assertEquals(DNSMapping.fromCode(-3007, "host.example"), EmileError.DNS.UnknownHost("host.example"): EmileError.DNS)
    assertEquals(DNSMapping.fromCode(-9999, "host.example"), EmileError.DNS.System(ErrorCode(-9999)): EmileError.DNS)
  }

  test("Unexpected returns an already-typed cause unwrapped") {
    assertEquals(EmileError.Bind.Unexpected(EmileError.Bind.PermissionDenied), EmileError.Bind.PermissionDenied: EmileError.Bind)
    assertEquals(EmileError.IO.Unexpected(EmileError.IO.ConnectionReset), EmileError.IO.ConnectionReset: EmileError.IO)
  }

  test("Unexpected wraps a foreign throwable with a derived message") {
    assertEquals(EmileError.Bind.Unexpected(new RuntimeException("boom")).getMessage, "Unexpected bind failure: boom")
  }

  test("SignalNumber rejects out-of-range numbers and accepts the boundary values") {
    assert(SignalNumber.from(0).isLeft)
    assert(SignalNumber.from(65).isLeft)
    assert(SignalNumber.from(1).isRight)
    assert(SignalNumber.from(64).isRight)
  }

  test("LoopConfig presets") {
    assert(!LoopConfig.default.blockProfilerSignal)
    assert(!LoopConfig.default.useIoUringSqpoll)
    assert(LoopConfig.profilerProfile.blockProfilerSignal)
  }

  test("System derives its message from the libuv error code") {
    val message = EmileError.Bind.System(ErrorCode(ErrorCode.UV_EADDRINUSE)).getMessage
    assert(message.startsWith("EADDRINUSE"), message)
  }

end EmileErrorSpec
