# Émile

> Named after the famed telegraph engineer, providing high-performance Scala 3 Native async I/O bindings backed by libuv.

[![Scala 3.8+](https://img.shields.io/badge/scala-3.8+-red.svg)](https://www.scala-lang.org/)
[![Scala Native 0.5+](https://img.shields.io/badge/scala--native-0.5+-blue.svg)](https://scala-native.org/)
[![cats-effect 3.7+](https://img.shields.io/badge/cats--effect-3.7+-green.svg)](https://typelevel.org/cats-effect/)

## Overview

Emile replaces cats-effect's default polling system with libuv. `LibuvPollingSystem` implements cats-effect's `PollingSystem` trait directly, making libuv the event loop that drives fibre scheduling.

- **Zero bridging overhead**: libuv callbacks wake fibres directly
- **Single shared loop**: All workers share one libuv loop with CAS-based serialisation
- **Full libuv surface**: TCP, DNS, timers, signals, file descriptor polling

## Architecture

```
emile-ipa      Cross-platform IP/port types (JVM, JS, Native)
emile-core     Native-only libuv FFI bindings
emile-cats     cats-effect integration (Resource, Eff, EmileIOApp)
```

## Quick Start

```scala
// build.sbt
libraryDependencies ++= Seq(
  "io.github.arashi01" %%% "emile-ipa"  % "<version>",  // Cross-platform
  "io.github.arashi01" %%% "emile-cats" % "<version>"    // Native only
)
```

### TCP Server

```scala
import cats.effect.*
import boilerplate.effect.*
import emile.cats.*
import emile.ipa.*
import emile.ipa.literals.*

object HelloServer extends EmileIOApp:
  def run(args: List[String]): IO[ExitCode] =
    val address = SocketAddress.v4(ipv4"0.0.0.0", port"8080")
    TcpResource.bind(address).use { server =>
      Eff.succeed[IO, EmileError, ExitCode](ExitCode.Success)
    }.rethrow
```

Resource factories acquire the loop internally - no explicit loop plumbing needed.

## Modules

### emile-ipa

Zero-allocation IP and socket address types with compile-time validation.

```scala
import emile.ipa.*
import emile.ipa.literals.*

val addr = ipv4"192.168.1.1"
val p = port"8080"
val v6 = ipv6"::1"
val socket = SocketAddress.v4(Ipv4Address.Loopback, Port(8080))
val parsed: Either[AddressError, SocketAddress] = SocketAddress.from("[::1]:443")
```

Types: `Port`, `Ipv4Address`, `Ipv6Address`, `SocketAddress`, `FlowInfo`, `ScopeId`.

### emile-core

Direct libuv bindings with phantom-state tracking (`Open`/`Closed`).

```scala
import emile.*

for
  loop   <- Loop.create
  timer  <- Timer.after(loop, Timeout.millis(100))(() => println("fired"))
  _      <- loop.run(RunMode.Default)
  _      <- loop.closeDrain
yield ()
```

Handle types: `Loop`, `Tcp[S]`, `Timer[S]`, `Async[S]`, `Poll[S]`, `SignalHandle[S]`.

### emile-cats

cats-effect integration with typed error channels via `Eff[IO, EmileError, A]`.

```scala
import cats.effect.*
import boilerplate.effect.*
import emile.cats.*

object MyApp extends EmileIOApp:
  def run(args: List[String]): IO[ExitCode] =
    TcpResource.make.use { tcp =>
      TimerResource.make.use { timer =>
        Eff.succeed[IO, EmileError, ExitCode](ExitCode.Success)
      }
    }.rethrow
```

Resource types: `TcpResource`, `TimerResource`, `AsyncResource`, `PollResource`, `DnsResolver`, `SignalStream`.

## Error Handling

All fallible operations return `Eff[IO, EmileError, A]` (zero-cost wrapper for `IO[Either[EmileError, A]]`):

```scala
import boilerplate.effect.*

val tcp: Eff[IO, EmileError, Tcp[Open]] = TcpResource.make.allocated.map(_._1)

// Convert to IO (raises EmileError as Throwable)
val io: IO[Tcp[Open]] = tcp.rethrow

// Pattern-match on errors
io.catchEmile {
  case EmileError.AlreadyClosed => IO.unit
}
```

Error types: `AddressError` (emile-ipa), `EmileError` (emile-core/cats).

## Signal Handling

Cross-platform signal handling via libuv:

```scala
import emile.cats.*

SignalStream.watch(Signal.SIGTERM).use { case (queue, ready) =>
  Eff.liftF(ready) *> Eff.liftF(queue.take)
}
```

Unix: full POSIX support. Windows: SIGINT, SIGBREAK, SIGHUP only.

## Requirements

- **Scala**: 3.8+
- **Scala Native**: 0.5+
- **libuv**: 1.x (system library)

## Building

```bash
sbt compile        # Compile
sbt test           # Run tests
sbt staticCheck    # Code style check
sbt format         # Auto-format
```

## Licence

Licensed under the [Apache Licence, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
