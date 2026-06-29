# Émile

Scala Native async I/O, built on libuv, integrated with cats-effect.

```scala
import com.comcast.ip4s.*
import emile.*

object EchoServer extends EmileIOApp.Simple:
  def runEff: EmIO[EmileError, Unit] =
    val addr = SocketAddress(ipv4"0.0.0.0", port"8080")
    Tcp.bind(addr, TcpOptions.server).widen[EmileError].use: server =>
      server.accepted
        .parEvalMapUnordered(4096): connection =>
          connection.use(socket => socket.reads.through(socket.writes).compile.drain)
        .compile.drain
```

Émile is the Scala Native event-loop integration cats-effect always wanted: one libuv `uv_loop_t` per work-stealing
worker, plugged into cats-effect's `PollingSystem` hook. Every TCP, DNS, timer, signal, file, and fd-poll operation
rides the same loop that schedules the fibre that issued it, with no `Future`/`Promise` indirection and no separate
executor.

It is **Native-first**: the public API is shaped for the Scala Native representation; the typed-error channel is
`boilerplate.effect.EffIO[+E, +A]`.

## Modules

| Module      | Artifact                        | Purpose                                                                                                            |
|-------------|---------------------------------|--------------------------------------------------------------------------------------------------------------------|
| `emile`     | `io.github.arashi01::emile`     | Core library: bootstrap, TCP, DNS, timers, signals, files, fd-polling.                                             |
| `emile-fs2` | `io.github.arashi01::emile-fs2` | fs2-networking interop: `TcpSocket.asFs2` / `TcpServer.acceptFs2` adapters onto `fs2.io.net.Socket[IO]`. Optional. |

```scala
// build.sbt (sbt 2.x)
libraryDependencies ++= List(
  "io.github.arashi01" %% "emile"     % "<version>",
  "io.github.arashi01" %% "emile-fs2" % "<version>",  // only if you need the fs2 adapter
)
```

## Public API

### Bootstrap

```scala
import emile.*

// Application entry point - libuv polling system installed automatically.
object MyApp extends EmileIOApp.Simple:
  def runEff: EmIO[EmileError, Unit] = ???

// Or, for an app that consumes process arguments and returns an ExitCode:
object MyArgsApp extends EmileIOApp:
  def runEff(args: List[String]): EmIO[EmileError, ExitCode] = ???

// Standalone, for non-IOApp code: runs the effect on a fresh libuv runtime, then shuts it down.
Emile.runEff(myEff)

// For an embedder owning the process lifecycle, `Emile.runtime` yields the runtime as a
// `Resource[IO, IORuntime]` (shut down on release).
Emile.runtime.use(rt => program(rt))
```

### TCP

```scala
import com.comcast.ip4s.*
import emile.*

// Server: bind + listen completes synchronously inside resource acquisition; any failure
// surfaces here, never later mid-stream.
Tcp.bind(SocketAddress(ipv4"0.0.0.0", port"8080"), TcpOptions.server).use: server =>
  // each accepted socket's lifetime is the handler's `use` scope - safe under every combinator.
  server.accepted.evalMap(_.use(handle)).compile.drain                  // serial
  server.accepted.parEvalMapUnordered(256)(_.use(handle)).compile.drain // unordered concurrency

// Client: connect to an address ...
Tcp.connect(SocketAddress(ipv4"127.0.0.1", port"8080")).use: socket =>
  socket.write(Chunk.array("ping".getBytes("UTF-8"))) >> socket.read(4096)

// ... or resolve and connect by hostname (serial attempts in resolver order).
Tcp.connect(host"example.com", port"443").use(socket => ???)
```

A `TcpSocket` offers four read paths chosen by two axes - one-shot vs persistent, copying vs zero-copy:

| Method     | Mode       | Buffer        | Typical use |
|---|---|---|---|
| `read`     | one-shot   | owned `Chunk` | request/response framing |
| `readN`    | one-shot   | owned `Chunk` | fixed-size header reads |
| `readPtr`  | one-shot   | zero-copy view | a single zero-copy read |
| `reads`    | persistent | owned `Chunk` (stream) | TLS handshakes, websockets, line protocols |
| `consume`  | persistent | zero-copy view | nghttp2-style synchronous chunk consumers |

Writes mirror: `write(chunk: Chunk[Byte])`, `writes: Pipe[..., Byte, Nothing]`, `writePtr(buf, len)` for an
already-native buffer, and `sendFile(file: OpenFile, offset, length)` for kernel-to-socket zero-copy via
`uv_fs_sendfile`. Half-closes are `endOfInput` (send-only) and `endOfOutput` (`uv_shutdown`-backed receive-only).

`socket.onLoop[A](thunk: => A)` submits a synchronous step to the socket's owning loop thread - the public face of
emile's worker-affinity routing, useful for thread-confining a piece of C state (e.g. an nghttp2 `nghttp2_session`)
alongside its read trampoline.

### Timer

`Timer.after(delay)` is a cancelable `IO.sleep` driven by cats-effect's per-worker `TimerHeap`; `LibuvPollingSystem`
arms a `uv_timer_t` to bound each loop iteration's wait, so sleeps fire on time. `Timer.interval(period)` is a
`fs2.Stream.fixedRate`.

### Signal

```scala
import emile.SignalNumber.*

Signal.watch(SIGUSR1).evalMap(_ => IO.println("got SIGUSR1")).compile.drain
Signal.termination.head.compile.drain // SIGINT|SIGTERM, once
```

A single supervisor worker owns one `uv_signal_t` per signum; every subscriber, on any worker, receives every delivery.
There is no `AlreadyWatched` failure mode and no race on the install.

### AsyncSignal

`AsyncSignal.resource` allocates a `uv_async_t`; `signal.fire` is libuv's documented cross-thread wake (no routing, no
copy), `signal.fires` is a coalesced wake-up stream. libuv may coalesce many `fire`s into one delivery - it is an
edge-triggered wake-up, not a counter.

### DNS

```scala
import com.comcast.ip4s.*

Dns.resolve(host"example.com", port"443"): EmIO[EmileError.Dns, NonEmptyList[SocketAddress[IpAddress]]]
Dns.resolve(host"example.com"):             EmIO[EmileError.Dns, NonEmptyList[IpAddress]]
Dns.reverse(ip"127.0.0.1"):                  EmIO[EmileError.Dns, Hostname]
```

emile deliberately does not publish a `given com.comcast.ip4s.Dns[IO]`: ip4s seals that abstraction. Code reaching for
`host.resolve[IO]` uses ip4s's own platform-resolver instance; emile's libuv resolver is reached through these calls,
which also carry the typed `EmileError.Dns` channel.

### FdPoll

```scala
FdPoll.resource(fd, Set(FdEvent.Readable, FdEvent.Disconnect)).use: poll =>
  poll.await.flatMap(events => ???)
```

Backed by `uv_poll_t` - one-shot readiness on a foreign file descriptor.

### OpenFile

```scala
OpenFile.open(java.nio.file.Paths.get("payload.bin")).use: file =>
  file.size.flatMap(sz => socket.sendFile(file, 0L, sz))
```

`OpenFile` wraps a `uv_file` for the `sendFile` zero-copy path. The same file may serve multiple `sendFile` calls (range
requests, partial-content responses) under one Resource scope.

## Typed errors as values

emile's effect alias is `EmIO[+E, +A] = boilerplate.effect.EffIO[E, A] = IO[Either[E, A]]` - covariant in both `E` and
`A`. Every fallible operation publishes its precise error type:

```scala
def bind(addr: SocketAddress[IpAddress]): EmResource[EmileError.Bind, TcpServer]
def connect(addr: SocketAddress[IpAddress]): EmResource[EmileError.Connect, TcpSocket]
def connect(host: Host, port: Port): EmResource[EmileError.HostConnect, TcpSocket]
def read(maxBytes: Int): EmIO[EmileError.Io, Option[Chunk[Byte]]]
```

`EmileError` is a sealed hierarchy with sub-traits per operation family (`Bind`, `Connect`, `Dns`, `Io`, `HostConnect`,
`Runtime`); each carries named domain cases plus a `System(code: ErrorCode)` for unanticipated libuv codes and an
`Unexpected(cause: Throwable)` for non-`EmileError` defects. `EmileError <: Exception`, so projecting onto plain `IO`'s
`Throwable` channel via `.absolve` is lossless - the carried value remains pattern-matchable.

The companion stream and resource aliases follow the same shape:

| Alias                  | = | Variance in `E` |
|---|---|---|
| `EmIO[+E, +A]`         | `EffIO[E, A]`            | covariant |
| `EmStream[+E, +A]`     | `Stream[EffIO.Of[E], A]` | covariant |
| `EmResource[E, A]`     | `Resource[EffIO.Of[E], A]` | invariant (widen with `r.widen[E2]`) |
| `EmPipe[E, -I, +O]`    | `Pipe[EffIO.Of[E], I, O]` | invariant; apply with `through` |

## Loop tuning

```scala
final case class LoopConfig(blockProfilerSignal: Boolean, useIoUringSqpoll: Boolean)

object LoopConfig:
  val default: LoopConfig          // conservative; SIGPROF not blocked, no SQPOLL
  val profilerProfile: LoopConfig  // blocks SIGPROF so a sampling profiler can drive the process
```

`LoopConfig` is applied per worker. SQPOLL is opt-in only: it needs `CAP_SYS_NICE` on kernels < 5.13 and falls back with
`ENOSYS` otherwise. Turn it on with `LoopConfig.default.copy(useIoUringSqpoll = true)`.

## fs2 interop - the `emile-fs2` module

Core `emile` depends on `fs2-core`. The optional `emile-fs2` module additionally pulls in `fs2-io` and adds two
adapters:

```scala
import emile.*
import emile.Fs2Interop.*

socket.asFs2: fs2.io.net.Socket[IO]                                  // a TcpSocket as fs2's Socket
server.acceptFs2: fs2.Stream[IO, fs2.io.net.Socket[IO]]              // accept stream onto IO
```

The adapters cover per-connection interop with the fs2 ecosystem (the s2n-tls `TLSContext` wrappers, byte-level codecs,
etc.). They project emile's typed-error channel onto `IO`'s `Throwable` channel via `EffIO.absolve` - lossless, since
`EmileError <: Exception`.

emile-fs2 deliberately does not implement `fs2.io.net.Network[IO]`. `Network[F]` is sealed in fs2; the principled path
to a libuv-backed `Network[IO]` is an upstream contribution exposing fs2's provider SPI, which is fs2's call to make.
Code that takes `Network[F]` as a constraint cannot be backed by emile through any short-of-upstream-changes path;
`asFs2` / `acceptFs2` cover everything that consumes a `Socket[IO]` or a `Stream[IO, Socket[IO]]`.

## Linking emile in your project

emile's `@extern` libuv bindings carry no `@link`, so the `-luv` linker option is supplied by the build. If you build
with **sbt-snx**, emile's published descriptor declares it and the option is folded into your link automatically - no
configuration needed. With the **official sbt-scala-native plugin**, add it to your own `nativeConfig` at `nativeLink`
time:

```scala
// build.sbt
libraryDependencies += "io.github.arashi01" %% "emile" % "<version>"

import scala.scalanative.build.NativeConfig
nativeConfig := nativeConfig.value.withLinkingOptions(
  nativeConfig.value.linkingOptions :+ "-luv"
)
```

For a fully-static binary, add `-static` alongside `-luv` and ensure your build environment provides a static libuv
archive (Alpine edge's `libuv-static` package does).

For building emile itself from source, see [CONTRIBUTING.md](CONTRIBUTING.md).

### libuv version

emile binds `uv_tcp_keepalive_ex`'s three-field form, which landed in **libuv 1.52.0** - your target machine needs
libuv >= 1.52.0 at runtime (dynamic) or build time (static). Distros currently shipping a recent enough libuv: **Fedora
rawhide**, **Alpine edge**. RHEL 10, Ubuntu 26.04, and Alpine 3.23.x still ship libuv 1.51.x.

## Caveats

### IPv6 scope id is not carried through `SocketAddress`

emile's `SocketAddress[IpAddress]` to C `sockaddr_in6` round-trip does not propagate the `sin6_scope_id` field. ip4s
models the scope id as `Ipv6Address.scopeId`, but the round-trip narrows it to `None`. A link-local IPv6 address (e.g.
`fe80::1%eth0`) therefore loses its scope across the conversion - bind / connect / accept will use scope id 0. If you
need scope-bound link-local IPv6 today, supply the scope through the underlying interface configuration; a future emile
release may carry it through explicitly.

### musl support is best-effort

emile's musl support is best-effort, pending upstream Scala Native runtime support for musl/Alpine that is still
landing ([scala-native#4934](https://github.com/scala-native/scala-native/pull/4934)); treat a musl target as
experimental until a Scala Native release carrying that work ships. Fully-static (`-static`) musl builds are the most
affected - prefer dynamic linking on musl today. glibc targets are unaffected.

## Platform Dependencies

| Component    | Required version                |
|--------------|---------------------------------|
| Scala        | **3.8.x**                       |
| Scala Native | **0.5.12+**                     |
| libuv        | **>= 1.52.0**                   |
| Platform     | Linux (glibc or musl)           |

## Licence

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full text.
