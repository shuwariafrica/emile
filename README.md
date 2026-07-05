# Émile

Scala Native async I/O, built on libuv, integrated with cats-effect.

```scala
import com.comcast.ip4s.*
import emile.*

object EchoServer extends EmileIOApp.Simple:
  def runEff: EmIO[EmileError, Unit] =
    val addr = SocketAddress(ipv4"0.0.0.0", port"8080")
    TCP.bind(addr, TCPOptions.server).widen[EmileError].use: server =>
      server.accepted
        .parEvalMapUnordered(4096): connection =>
          connection.use(socket => socket.reads.through(socket.writes).compile.drain)
        .compile.drain
```

Émile is the Scala Native event-loop integration cats-effect always wanted: one libuv `uv_loop_t` per work-stealing
worker, plugged into cats-effect's `PollingSystem` hook. Every TCP, IPC, DNS, timer, signal, file, filesystem-watch,
and fd-poll operation runs on the loop thread that owns its handle - the worker the resource was acquired on - with no
`Future`/`Promise` indirection and no separate executor.

It is **Native-first**: the public API is shaped for the Scala Native representation; the typed-error channel is
`boilerplate.effect.EffIO[+E, +A]`.

## Modules

| Module      | Artifact                        | Purpose                                                                                                            |
|-------------|---------------------------------|--------------------------------------------------------------------------------------------------------------------|
| `emile`     | `africa.shuwari::emile`     | Core library: bootstrap, TCP, IPC (Unix-domain sockets), DNS, timers, signals, files, filesystem watching, fd-polling. |
| `emile-fs2` | `africa.shuwari::emile-fs2` | fs2-networking interop: `TCPSocket.asFs2` / `TCPServer.acceptFs2` adapters onto `fs2.io.net.Socket[IO]`. Optional. |

```scala
// build.sbt (sbt 2.x)
libraryDependencies ++= List(
  "africa.shuwari" %% "emile"     % "<version>",
  "africa.shuwari" %% "emile-fs2" % "<version>",  // only if you need the fs2 adapter
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

// Standalone, for non-IOApp code: runs the effect on a fresh libuv runtime (shut down afterwards) and
// returns the typed result - Left(EmileError) on failure, Right on success.
val result: Either[EmileError, Unit] = Emile.runEff(myEff)

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
TCP.bind(SocketAddress(ipv4"0.0.0.0", port"8080"), TCPOptions.server).use: server =>
  // each accepted socket's lifetime is the handler's `use` scope - safe under every combinator.
  server.accepted.evalMap(_.use(handle)).compile.drain                  // serial
  server.accepted.parEvalMapUnordered(256)(_.use(handle)).compile.drain // unordered concurrency

// Client: connect to an address ...
TCP.connect(SocketAddress(ipv4"127.0.0.1", port"8080")).use: socket =>
  socket.write(Chunk.array("ping".getBytes("UTF-8"))) >> socket.read(4096)

// ... or resolve and connect by hostname (serial attempts in resolver order).
TCP.connect(host"example.com", port"443").use(socket => ???)
```

A connected socket offers five read methods across two axes - one-shot vs persistent, copying vs zero-copy:

| Method     | Mode       | Buffer        | Typical use |
|---|---|---|---|
| `read`     | one-shot   | owned `Chunk` | request/response framing |
| `readN`    | one-shot   | owned `Chunk` | fixed-size header reads |
| `readPtr`  | one-shot   | zero-copy view | a single zero-copy read |
| `reads`    | persistent | owned `Chunk` (stream) | TLS handshakes, websockets, line protocols |
| `consume`  | persistent | zero-copy view | feeding a synchronous native parser or codec |

The read modes share one per-socket buffer, so a socket has a single reader: starting a read while another is in
flight fails fast with `EmileError.IO.ConflictingOperation`. Reading and writing concurrently is fine - they are
independent directions.

Writes mirror the reads: `write(chunk: Chunk[Byte])`, `write(chunks: Seq[Chunk[Byte]])` for one ordered gathering
write, `writes: Pipe[..., Byte, Nothing]`, `writePtr(buf, len)` for an already-native buffer, `tryWritePtr(buf, len)`
for a synchronous best-effort write, and `sendFile(file: OpenFile, offset, length)` for a kernel-to-socket transfer
(see [OpenFile](#openfile)). Half-closes are `endOfInput` (send-only) and `endOfOutput` (`uv_shutdown`-backed
receive-only); `closeReset` (TCP only) aborts the connection with a RST rather than a graceful FIN, discarding any
queued output. Concurrent writes from different fibres are safe but their relative order is unspecified; batch them
into one `write(chunks)` or use a single writer where order matters.

`socket.onLoop` runs a synchronous step on the socket's owning loop thread - where thread-unsafe C state, such as a
stateful native protocol codec, must live, since that is the one thread driving the socket's I/O. Keep it short and
non-blocking: it stalls that worker's I/O until it returns.

### IPC (Unix-domain sockets)

```scala
import emile.*

// Server on the Linux abstract namespace - no filesystem entry, nothing to clean up.
IPC.bind(IPCAddress.Abstract("my-service")).use: server =>
  server.accepted.parEvalMapUnordered(64)(_.use(handle)).compile.drain

// Client.
IPC.connect(IPCAddress.Abstract("my-service")).use: socket =>
  socket.write(Chunk.array("ping".getBytes("UTF-8"))) >> socket.read(4096)
```

`IPC` mirrors `TCP`: a bound `IPCServer` yields the same `accepted` stream, and an `IPCSocket` shares the byte-stream
surface documented under [TCP](#tcp) (`reads`, `writes`, `read`, `sendFile`, the half-closes). The address is an
`IPCAddress` rather than an IP one:

| `IPCAddress`     | Meaning                                                                                  |
|------------------|------------------------------------------------------------------------------------------|
| `Path(value)`    | a filesystem socket file - libuv removes it on close                                     |
| `Abstract(name)` | a Linux abstract-namespace name - no filesystem entry, no residue                        |
| `Autobind`       | bind only: the kernel assigns an abstract name, reported back as `Abstract` once bound   |

A name longer than the platform `sun_path` limit (about 108 bytes) is rejected, not silently bound to a truncated,
different socket.

Two operations are IPC-only:

```scala
socket.peerCredentials          // EmIO[EmileError.IO, PeerCredentials] - the peer's (processId, userId, groupId)
server.chmod(IPCMode.ReadWrite) // EmIO[EmileError.IO, Unit] - set the socket file's access mode
```

`peerCredentials` reads the connected peer through `SO_PEERCRED`, for a server to authorise a local client. `chmod`
applies to a `Path` server only; connecting needs write access, so `Writable` or `ReadWrite` opens a server to other
local users.

To harden a socket from the outset, set the mode at bind - `IPC.bind(path, IPCOptions(mode = Some(IPCMode.Readable),
listenBacklog = 128))` applies it before the server listens, so the socket is never briefly reachable at a wider mode;
`server.chmod` re-sets it on an already-bound server. A mode on an abstract or autobind address (which has no socket
file) is rejected.

### Serving connections

`server.serve` runs a complete accept-and-handle loop with graceful shutdown - the piece that is genuinely hard to
assemble from raw fs2. It works for both `TCP` and `IPC` servers.

```scala
import cats.effect.{Deferred, IO}
import emile.*

// A signal the caller completes to begin a graceful stop.
Deferred[IO, Unit].flatMap: shutdown =>
  val echo = (socket: TCPSocket) => socket.reads.through(socket.writes).compile.drain
  server.serve(4096, shutdown.get)(err => IO.println(s"connection failed: $err"))(echo)
```

Up to `maxConcurrent` handlers run at once. A failure in one handler - or in the accept loop - is reported through
`onError` without stopping the server or its siblings; a handler defect arrives there too, as `EmileError.IO.Unexpected`
(`onError`'s argument is the [union channel](#bringing-your-own-error-type)). Completing `shutdown` stops accepting and
**drains** - in-flight handlers run to completion, they are not cancelled - so a handler that never finishes holds
shutdown open; bound it with `.timeout` where needed. The socket is released when its handler finishes. For a server
that stops on the first failure, compose `accepted` directly, as in the [opening example](#émile).

`Socket.proxy(a, b)` splices two connected sockets into a bidirectional pipe for reverse-proxy and sidecar work. Each
direction is copied, and a half-close on one side is propagated to the other, so a client shutdown reaches the
upstream; the pipe completes once both directions close. The two sockets may be of different kinds - a TCP front
spliced to an `IPC` backend.

```scala
Socket.proxy(frontConnection, backendConnection): EmIO[EmileError.IO, Unit]
```

### Timer

`Timer.after(delay)` is a cancelable `IO.sleep` driven by cats-effect's per-worker `TimerHeap`; `LibUVPollingSystem`
arms a `uv_timer_t` to bound each loop iteration's wait, so sleeps fire on time. `Timer.interval(period)` is a
`fs2.Stream.fixedRate`.

### Signal

```scala
import boilerplate.effect.EffIO
import cats.effect.IO
import emile.SignalNumber.*

Signal.watch(SIGUSR1).evalMap(_ => EffIO.liftF(IO.println("got SIGUSR1"))).compile.drain
Signal.termination.head.compile.drain // SIGINT|SIGTERM, once
```

Within each runtime, one supervisor worker owns one `uv_signal_t` per signum; every subscriber, on any worker, receives
every delivery. There is no `AlreadyWatched` failure mode and no race on the install, and a fresh runtime in the same
process installs its own handlers.

### AsyncSignal

`AsyncSignal.resource` gives a cross-fibre / cross-thread wake-up - a cats-effect-backed convenience (typed errors, a
`Resource` lifecycle, coalescing), not a libuv handle. `signal.fire`, from any fibre or thread, surfaces on the
`signal.fires` stream. A capacity-one circular buffer coalesces: rapid fires collapse to a single pending wake-up - an
edge-triggered signal, not a counter. `fires` is drained by a single subscriber; a second concurrent consumer fails
fast with `EmileError.IO.ConflictingOperation`.

### DNS

```scala
import com.comcast.ip4s.*

DNS.resolve(host"example.com", port"443"): EmIO[EmileError.DNS, NonEmptyList[SocketAddress[IpAddress]]]
DNS.resolve(host"example.com"):             EmIO[EmileError.DNS, NonEmptyList[IpAddress]]
DNS.reverse(ip"127.0.0.1"):                  EmIO[EmileError.DNS, Hostname]
```

emile deliberately does not publish a `given com.comcast.ip4s.Dns[IO]`: ip4s seals that abstraction. Code reaching for
`host.resolve[IO]` uses ip4s's own platform-resolver instance; emile's libuv resolver is reached through these calls,
which also carry the typed `EmileError.DNS` channel.

### FDPoll

```scala
FDPoll.resource(fd, Set(FDEvent.Readable, FDEvent.Disconnect)).use: poll =>
  poll.await                 // EmIO[EmileError.IO, Set[FDEvent]] - the next readiness, once
  poll.awaits.compile.drain  // EmStream[EmileError.IO, Set[FDEvent]] - readiness, re-armed per element
```

Backed by `uv_poll_t` - readiness on a foreign file descriptor that emile does not own (the descriptor must stay
open for the resource's lifetime). `await` is one-shot; `awaits` holds the same handle across deliveries, for a
descriptor watched repeatedly without re-acquiring. `uv_poll` is level-triggered, so `awaits` disarms between
elements and re-arms on the next pull - a slow consumer cannot busy-loop the poll, and re-arming still catches a
descriptor that is already ready. Readiness is a single shared condition, so a watcher serves one waiter: do not run
`await` / `awaits` concurrently on one watcher. Using the watcher after its resource has released is a typed
`EmileError.IO.AlreadyClosed`.

### FS

```scala
import boilerplate.effect.EffIO
import cats.effect.IO
import emile.*

// Reload-on-change: re-read the path on each coalesced pulse (watch a directory, see the caveat below).
FS.watch(java.nio.file.Path.of("/etc/myapp")).use: watcher =>
  watcher.changes.evalMap(_ => EffIO.liftF(IO.println("config changed - reloading"))).compile.drain
```

`FS.watch` uses inotify (`uv_fs_event_t`); `FS.poll(path, interval)` stat-polls instead, for paths inotify cannot serve
(network filesystems, some container mounts). Both expose the changes two ways, each drained by a single subscriber (a
second concurrent consumer fails fast with `EmileError.IO.ConflictingOperation`):

| Stream    | Element   | Use |
|-----------|-----------|-----|
| `events`  | `FSEvent` | every change, in order - the lossless detail view |
| `changes` | `Unit`    | one coalesced pulse per burst - the re-scan trigger for high churn |

Each `FSEvent` carries the `Set[FSChange]` observed - `Renamed` (an entry created, deleted, moved, or renamed) and/or
`Changed` (an entry's contents or attributes) - and, when the platform supplies one, the affected entry's `filename`.
A libuv watch error ends either stream on its typed `EmileError.IO` channel.

Filesystem watching is inherently lossy - the kernel can drop events on overflow, and an inotify watch detaches when
its file is replaced by rename - so prefer watching a **directory** over a single file, and use `changes` + a re-scan
where you must not miss a change (a re-scan reads current state regardless of how many notifications coalesced).
`events` is lossless only while the consumer keeps up; for sustained high churn use `changes`, which keeps at most one
pulse outstanding. The platform also coalesces rapid changes and may omit the entry name, so any further debouncing is
the consumer's concern. `FS.poll` is coarser still: it reports only the watched path's own stat transitions (no entry
name), so polling a directory misses changes within its files.

### OpenFile

```scala
OpenFile.open(java.nio.file.Path.of("payload.bin")).use: file =>
  file.read(65536)  // EmIO[EmileError.IO, Option[Chunk[Byte]]] - advances the position; None at EOF
  file.reads        // EmStream[EmileError.IO, Byte] - the whole file from the current position
  file.size         // EmIO[EmileError.IO, Long]
```

`OpenFile` wraps a read-only `uv_file`; the descriptor closes when the Resource releases, and one open file may serve
many reads. To send a file to a socket, choose by need:

```scala
file.reads.through(socket.writes)  // whole-file body, fully backpressured (uv_write, one copy)
socket.sendFile(file, 0L, size)    // single uv_fs_sendfile syscall, zero-copy, best-effort
```

`sendFile` is a single `uv_fs_sendfile` syscall: it returns the bytes actually sent, which may be fewer than `length`
(0 when the socket send buffer is full). It writes the raw descriptor outside libuv's write queue, so it must not
overlap an in-flight `write` or an `endOfOutput` half-close on the same socket - a concurrent one fails fast with
`EmileError.IO.ConflictingOperation`. For a complete, backpressured body, prefer `file.reads.through(socket.writes)`.

## Timeouts, retries, and limits

emile ships no timeout, retry, rate-limit, or connection-pool wrappers - each composes from the cats-effect and fs2
substrate the typed-error channel already rides:

- **Timeout** - `eff.timeout(5.seconds, onTimeout)` bounds a typed effect, raising the error you name; or
  `eff.absolve.timeout(5.seconds)` on the plain `IO` for a `TimeoutException`. Wrap a connect, a single read, or a whole
  `serve` handler.
- **Retry** - `EffIO.retry(TCP.connect(addr).use(run), maxRetries = 3)` re-runs a typed effect on failure.
- **Connection limit** - `accepted.parEvalMapUnordered(n)(_.use(handler))`, or `serve(n, ...)`, caps concurrent handlers
  at `n`.
- **Ephemeral port** - bind to port `0`, then read the kernel-assigned port back from `server.address`.
- **Rate limiting and batching** - any fs2 `Stream` combinator (`metered`, `groupWithin`, ...) over `accepted` or
  `reads`.

## Typed errors as values

emile's effect alias is `EmIO[+E, +A] = boilerplate.effect.EffIO[E, A] = IO[Either[E, A]]` - covariant in both `E` and
`A`. Every fallible operation publishes its precise error type:

```scala
def bind(addr: SocketAddress[IpAddress]): EmResource[EmileError.Bind, TCPServer]
def connect(addr: SocketAddress[IpAddress]): EmResource[EmileError.Connect | EmileError.IO, TCPSocket]
def connect(host: Host, port: Port): EmResource[EmileError.HostConnect | EmileError.IO, TCPSocket]
def read(maxBytes: Int): EmIO[EmileError.IO, Option[Chunk[Byte]]]
```

A `connect` result is a union: the connect itself fails with `Connect` (or `HostConnect` for the hostname overload),
while a failure to apply `TCPOptions` to the now-established socket surfaces as the `EmileError.IO` it is - a
socket-option error, not a connect one.

`EmileError` is a sealed hierarchy with sub-traits per operation family (`Bind`, `Connect`, `DNS`, `IO`, `HostConnect`,
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

### Bringing your own error type

`readPtr`, `consume`, `onLoop`, and `serve` take a callback that fails with an error of your choosing - any `Throwable`,
so an ADT extending `Exception`. emile keeps that error and its own on the union channel `EmileError.IO | E`, which you
match arm by arm and which stays `.absolve`-able throughout:

```scala
enum AppError extends Exception:
  case Rejected
  case Malformed(at: Int)

val consumed: EmIO[EmileError.IO | AppError, Unit] =
  socket.consume((ptr, len) => if len > 0 then Right(()) else Left(AppError.Rejected))

consumed.either.map:
  case Left(io: EmileError.IO) => // a socket read failed
  case Left(app: AppError)     => // onChunk returned Left
  case Right(())               => // reached end of input
```

To recover just your own error and leave emile's propagating, `catchOnly` narrows the channel back to the residual -
here `EmileError.IO`, inferred from the union:

```scala
val recovered: EmIO[EmileError.IO, Unit] =
  consumed.catchOnly((app: AppError) => EffIO.succeed(()))   // AppError handled; a read failure stays typed
```

`catchOnly` (recover one arm of a union error), `catchSome` (recover the errors a `PartialFunction` handles, the rest
pass through), and the other typed-error combinators come from `boilerplate.effect`.

Since `EmileError.IO | EmileError.IO` is just `EmileError.IO`, a callback whose own errors are `EmileError.IO` leaves
you a plain `EmIO[EmileError.IO, A]` to match - no union to unwrap.

## Loop tuning

```scala
final case class LoopConfig(blockProfilerSignal: Boolean, useIoUringSqpoll: Boolean)

object LoopConfig:
  val default: LoopConfig          // conservative; SIGPROF not blocked, no SQPOLL
  val profilerProfile: LoopConfig  // blocks SIGPROF so a sampling profiler can drive the process
```

`LoopConfig` is applied per worker. SQPOLL is opt-in only: it needs `CAP_SYS_NICE` on kernels < 5.13 and falls back with
`ENOSYS` otherwise. Turn it on with `LoopConfig.default.copy(useIoUringSqpoll = true)`.

Separately, DNS resolution and all file I/O run on libuv's **process-wide** worker threadpool - one pool shared by every
loop, four threads by default. There is no per-loop or runtime knob: the pool is a single process resource, sized once
from the `UV_THREADPOOL_SIZE` environment variable, read before the first such operation (maximum 1024). Raise it when
concurrent name resolution or file I/O is the bottleneck.

## Observability

emile's socket and stream I/O surfaces in cats-effect's runtime metrics: each worker's libuv loop reports its operations
as a `PollerMetrics`, reachable through `runtime.metrics.workStealingThreadPool`.

```scala
val loops = runtime.metrics.workStealingThreadPool.toList.flatMap(_.workerThreads)
loops.map(_.poller.totalReadOperationsSucceededCount()).sum   // reads delivered across every loop
```

Counts are grouped by category - `accept`, `connect`, `read`, `write` - each with submitted / succeeded / errored /
canceled totals. `read` and `accept` are counted as they complete (a persistent read has no single in-flight
operation), while `write` and `connect` also carry an outstanding count. Threadpool work (DNS and file I/O) has no
`PollerMetrics` category and is not counted.

## fs2 interop - the `emile-fs2` module

Core `emile` depends on `fs2-core`. The optional `emile-fs2` module additionally pulls in `fs2-io` and adds two
adapters:

```scala
import emile.*
import emile.Fs2Interop.*

socket.asFs2: fs2.io.net.Socket[IO]                                  // a TCPSocket as fs2's Socket
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
libraryDependencies += "africa.shuwari" %% "emile" % "<version>"

import scala.scalanative.build.NativeConfig
nativeConfig := nativeConfig.value.withLinkingOptions(
  nativeConfig.value.linkingOptions :+ "-luv"
)
```

For a fully-static binary, add `-static` alongside `-luv` and ensure your build environment provides a static libuv
archive (Alpine's `libuv-static` package provides one).

For building emile itself from source, see [CONTRIBUTING.md](CONTRIBUTING.md).

### libuv version

emile needs **libuv >= 1.51** at runtime (dynamic) or build time (static) - the version RHEL 10, Ubuntu 26.04, and
Alpine 3.23 ship, along with Fedora and Alpine edge. Install the runtime package to run, or the `-dev` / `-devel`
package to link (`apt install libuv1-dev`, `dnf install libuv-devel`, `apk add libuv-dev`).

## Caveats

### musl support is best-effort

emile's musl support is best-effort, pending upstream Scala Native runtime support for musl/Alpine that is still
landing ([scala-native#4934](https://github.com/scala-native/scala-native/pull/4934)); treat a musl target as
experimental until a Scala Native release carrying that work ships. Fully-static (`-static`) musl builds are the most
affected - prefer dynamic linking on musl today. glibc targets are unaffected.

### File writing goes through fs2

`OpenFile` is read-only - emile has no native file-write capability yet. For writing, creating, or truncating files,
use `fs2.io.file` (the `emile-fs2` module already brings fs2 in). Native file operations are tracked for a future
release.

### `connect(host, port)` is serial

`TCP.connect(host, port)` resolves the host and tries the addresses in resolver order, taking the first that connects -
not Happy Eyeballs / parallel racing. For a different strategy, resolve with `DNS.resolve` and compose the per-address
`TCP.connect(address)` primitive yourself.

## Platform Dependencies

| Component    | Required version                |
|--------------|---------------------------------|
| Scala        | **3.8.x**                       |
| Scala Native | **0.5.12+**                     |
| libuv        | **>= 1.51**                     |
| Platform     | Linux (glibc or musl)           |

## Licence

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full text.
