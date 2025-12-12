# Emile

*> Ultra-low overhead Scala 3 bindings for libuv with effect-specific ergonomics.*

This README targets library users. For contributor notes, see `stage_audit.md` and `api_proposal_v1.md`.

## Contents
- [Project Goals](#project-goals)
- [Architecture Overview](#architecture-overview)
- [Module Guides](#module-guides)
  - [emile-ipa](#emile-ipa)
  - [emile-core](#emile-core)
  - [emile-cats](#emile-cats)
  - [emile-zio](#emile-zio)
- [Effect System Comparison](#effect-system-comparison)
- [Development Status](#development-status)

## Project Goals
- Zero-overhead, allocation-aware Scala Native bindings for libuv (loop, TCP, timers, async, poll).
- Cross-platform IP/socket primitives usable from Scala Native, JVM, and Scala.js clients.
- First-class integrations for cats-effect and ZIO without reimplementing their lifecycle/cancellation semantics.
- Errors surfaced as typed `EmileError` values (never throw for control flow).
- Safety through opaque types, compile-time literals, and interop correctness verified against libuv sources.

## Architecture Overview
```
┌──────────────┐    ┌───────────────┐    ┌───────────────┐
│ emile-ipa    │──► │  emile-core   │──► │ cats / zio    │
│ (addresses)  │    │ (libuv FFI)   │    │ integrations  │
└──────────────┘    └───────────────┘    └───────────────┘
```
- `emile-ipa`: pure Scala3 addressing primitives shared by all back-ends.
- `emile-core`: Scala Native libuv wrappers with state-tracked handles.
- `emile-cats`: cats-effect runtime integration via a custom `PollingSystem`.
- `emile-zio`: ZIO service & scoped resources for manual loop driving.

Each module is ship-ready on its own; downstream apps depend only on the modules they need.

## Module Guides

### emile-ipa
Zero-allocation IP and socket address types used by the rest of the stack. Implemented as opaque types with compile-time literal validation and platform shims.

#### Key Features
- `Ipv4Address`, `Ipv6Address`, `Port`, and `SocketAddress` as opaque value classes.
- String interpolators (`ipv4"..."`, `ipv6"..."`, `port"..."`) validated at compile time.
- Native/JVM/JS conversion helpers (e.g. `SocketAddress.toSockAddr`, `toInetSocketAddress`).
- Precise error reporting via `AddressError` ADT.

#### Typical Workflow
1. Parse or construct addresses using runtime `Either`. 
2. Pass to `emile-core` TCP calculators without copying.
3. Convert back to platform-specific representations when interacting with legacy APIs.

#### Example
```scala
import io.github.arashi01.emile.ipa.*
import io.github.arashi01.emile.ipa.literals.*

val addresses = List("10.0.0.1:8080", "bad", "[::1]:443").map(SocketAddress.fromString)
val validated = addresses.traverse(identity) // Either[List[AddressError], List[SocketAddress]]
validated match
  case Left(errors) => errors.foreach(err => println(s"invalid: ${err.message}"))
  case Right(sockAddrs) => sockAddrs.foreach(addr => println(addr.show))
```

### emile-core
Scala Native bindings over libuv handles with compile-time state witnesses and zero-cost interop.

#### Key Features
- `Loop`, `Tcp`, `Timer`, `Async`, and `Poll` handles with phantom-state tracking (`Open`/`Closed`).
- `EmileConfig` for declarative loop/TCP configuration.
- `EmileError` ADT covering libuv errors, allocation failures, and validation issues.
- Safe callback registry with typed retrieval.

#### Basic Usage
1. Acquire a loop (`Loop.create`) or reuse the default loop.
2. Allocate handles (e.g. `Tcp.init`) and operate entirely through `Either[EmileError, *]`.
3. Close handles via `Handle` type class (`close`, `closeAsync`).

#### Example
```scala
import io.github.arashi01.emile.*
import io.github.arashi01.emile.ipa.literals.*
import io.github.arashi01.emile.ipa.SocketAddress

val server = for
  loop <- Loop.create
  tcp <- Tcp.init(loop)
  _ <- tcp.bind(SocketAddress.any(port"8080"))
  _ <- tcp.listen(backlog = 128) { status => println(s"accept: $status") }
  _ <- loop.run(RunMode.Default)
yield ()

server.left.foreach(err => println(s"boot failed: ${err.message}"))
```

### emile-cats
cats-effect integration that makes libuv the runtime poller.

#### Design Diagram
```
┌────────────────────────────────────────┐
│ cats-effect Runtime                    │
│  ┌──────────────┐  ┌───────────────┐  │
│  │ Worker Thread│  │ Worker Thread │  │
│  │  (libuv loop)│  │  (libuv loop) │  │
│  └──────┬───────┘  └──────┬────────┘  │
│         │ poll() -> uv_run│           │
│         │ interrupt -> async send    │
└─────────┴─────────────────────────────┘
```
- `LibuvPollingSystem` registers libuv loops per worker.
- `EmileLoop.integrated` exposes the owning loop as a `Resource`.
- `TcpResource`, `TimerResource`, `AsyncResource`, `PollResource` wrap handle lifecycles in `Resource`.

#### Error Handling & Accumulation
- All resource constructors lift `Either[EmileError, A]` into `IO[A]`.
- Compose validation with `cats.data.ValidatedNec` or `EitherT` to accumulate address/IO configuration errors before hitting IO.

#### Example (with error accumulation)
```scala
import cats.effect.{IO, IOApp}
import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.arashi01.emile.cats.*
import io.github.arashi01.emile.ipa.literals.*

object CatsServer extends EmileIOApp:
  override def run(args: List[String]): IO[ExitCode] =
    EmileLoop.integrated.use { loop =>
      given Loop = loop
      val addrV: ValidatedNec[String, SocketAddress] =
        SocketAddress.fromString("127.0.0.1:8080").leftMap(_.message).toValidatedNec

      addrV.toEither match
        case Left(errors) => IO.raiseError(new RuntimeException(errors.mkString_))
        case Right(addr) =>
          TcpResource.make.use { server =>
            IO.fromEither(server.bind(addr)) *> IO.never
          }
    }.as(ExitCode.Success)
```

### emile-zio
ZIO integration exposing the libuv loop as a service and resource wrappers via `Scope`.

#### Design Diagram
```
┌──────────────────────────────────────────┐
│ ZIO Runtime                              │
│  ┌────────────────────────────────────┐  │
│  │ User Program (ZIO)                 │  │
│  │  ┌──────────────┐   ┌────────────┐ │  │
│  │  │ EmileLoop    │   │ TcpResource│ │  │
│  │  │ (ZLayer)     │   │ etc.       │ │  │
│  │  └──────────────┘   └────────────┘ │  │
│  └────────────────────────────────────┘  │
│        loop.runUntilComplete driven manually │
└──────────────────────────────────────────┘
```
- `EmileLoop.scoped` acquires/lib releases loops via ZLayer/Scope.
- `TcpResource`, `TimerResource`, `AsyncResource` wrap handles with `ZIO.acquireRelease`.
- Users drive the loop via `EmileLoop.runUntilComplete` or custom fibres.

#### Error Handling & Accumulation
- All constructors return `ZIO[EmileError, *]`.
- Mix validation via `zio.prelude.Validation` or `ZValidation` before entering effectful blocks to accumulate configuration errors.

#### Example (with ZValidation)
```scala
import zio.*
import zio.prelude.*
import io.github.arashi01.emile.zio.*
import io.github.arashi01.emile.ipa.SocketAddress

val addrValidation = ZValidation.fromEither(SocketAddress.fromString("[::1]:8080").left.map(_.message))

val server = for
  addr <- addrValidation.toZIO
  tcp  <- TcpResource.bind(addr)
  _    <- ZIO.fromEither(tcp.listen(128)(_ => ()))
  loop <- EmileLoop.loop
  _    <- EmileLoop.runUntilComplete
yield ()

val programme = server.provideSomeLayer[Scope](EmileLoop.scoped)
```

## Effect System Comparison

| Dimension | emile-cats | emile-zio |
|-----------|------------|-----------|
| Event loop ownership | Runtime-managed via `LibuvPollingSystem` | User-managed via `EmileLoop` service |
| Polling integration | libuv is **the** poller (IO runtime replacement) | libuv runs alongside ZIO’s scheduler |
| Resource lifecycle | cats-effect `Resource` finalisers await libuv close callbacks | `ZIO.acquireRelease` + `Scope` await callbacks |
| Error channel | `Either[EmileError, *]` lifted into `IO` | Typed `EmileError` in `ZIO[EmileError, *]` |
| Error accumulation | `ValidatedNec`, `EitherT` helpers | `ZValidation`, `Validation` from Prelude |
| Loop driving | Automatic (runtime threads) | Manual (user must run loop fibre) |

## Development Status
- `emile-ipa`: Ready for multi-platform consumption; more IPv6 scope work planned.
- `emile-core`: Functional for Loop/Tcp/Timer/Async/Poll; awaiting multi-loop safety improvements.
- `emile-cats`: Experimental but viable; needs formal FS2 story and loop ownership diagnostics.
- `emile-zio`: Experimental; requires loop-driving helper and stream-level integrations.

Refer to `stage_audit.md` for outstanding issues and verification plans.

