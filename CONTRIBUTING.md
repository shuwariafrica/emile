# Contributing to Émile

This document covers what you need to build Émile from source, run its tests, and submit changes.

## Prerequisites

- **JDK 17 or newer.** CI uses JDK 17; any modern JDK works locally.
- **An sbt launcher** (`sbt` on your `$PATH`). `project/build.properties` pins emile's sbt version; the launcher fetches
  it automatically. No system-wide sbt-2.x install is required.
- **Native toolchain.** `clang` (16 or newer recommended) and `lld`. Émile is Linux-only.
- **For the vendored-libuv fallback only**: `cmake` toolchain. Not needed when your host already ships libuv 1.52.0+.

## Build flags

Two keys provided by the `EmileNativeBuild` AutoPlugin control how emile's tests are linked:

| Setting          | Env-var shortcut          | Default | Effect                                                                                                                                  |
|------------------|---------------------------|---------|-----------------------------------------------------------------------------------------------------------------------------------------|
| `useSystemLibUV` | `EMILE_SYSTEM_LIBUV=true` | `false` | `true` links against the host/distro libuv (`-luv`); `false` has sbt-snx clone and cmake-build a pinned libuv, linking the archive.     |
| `staticTestLink` | `EMILE_STATIC_LINK=true`  | `false` | `true` adds `-static`, producing a fully static test binary **on a musl toolchain (Alpine)**; ignored on glibc, where it stays dynamic. |

The two settings combine to four linkage modes (`-static` is honoured only on musl):

| `useSystemLibUV`  | `staticTestLink`  | Linkage                                                          |
|-------------------|-------------------|------------------------------------------------------------------|
| `false` (default) | `false` (default) | Vendored archive, dynamic - the contributor fallback.            |
| `false`           | `true`            | Vendored archive; `-static` on musl, dynamic on glibc.           |
| `true`            | `false`           | Distro libuv `-luv`, dynamic - **what Maven Central users get**. |
| `true`            | `true`            | Distro libuv `-luv`; `-static` (Alpine `libuv-static`) on musl.  |

## The vendored-libuv stopgap

emile binds `uv_tcp_keepalive_ex`'s three-field form, which landed in libuv **1.52.0**. The current distro-libuv
landscape:

| Distro                                 | Ships libuv | Tested in CI                                               |
|----------------------------------------|-------------|------------------------------------------------------------|
| RHEL 10 / Ubuntu 26.04 / Alpine 3.23.x | 1.51.x      | no (too old)                                               |
| Fedora rawhide                         | >= 1.52.0   | yes (`shuwariafrica/rawhide-jdk:17`)                       |
| Alpine edge                            | >= 1.52.0   | yes (`shuwariafrica/alpine-edge-jdk:17`, dynamic + static) |

When `useSystemLibUV` is `false` (the default), sbt-snx clones libuv (pinned to **1.52.1**) and cmake-builds a static
archive, linked into the test binary directly; no system libuv is touched, and no git submodule or manual checkout is
needed - `cmake` and `clang` are the only extra prerequisites. This is a **stopgap** - until mainstream distros ship
libuv 1.52+, at which point the vendored build can be dropped in favour of the system libuv.

## Running tests locally

### On a host with libuv >= 1.52.0 (rawhide, Alpine edge)

```bash
EMILE_SYSTEM_LIBUV=true sbt "emile/testOnly *" "emile-fs2/testOnly *"
```

### With the vendored fallback (any host with cmake + clang)

```bash
sbt "emile/testOnly *" "emile-fs2/testOnly *"
```

(`useSystemLibUV` defaults to `false`, so sbt-snx clones and cmake-builds libuv automatically - no submodule step.)

### Concurrency stress suite

`emile-stress` holds the concurrency-invariant suites (e.g. `AffinitySpec`), off the default aggregate and run
explicitly. They build their own forced aggressive auto-cede runtime so scheduler races surface (the default
thresholds hide them). CI runs them on every change (the `stress` job); locally:

```bash
EMILE_SYSTEM_LIBUV=true sbt "emile-stress/testOnly *"
```

### Reproducing a CI cell verbatim with Docker

```bash
DOCKER_IMAGE=shuwariafrica/alpine-edge-jdk:17 \
  EMILE_SYSTEM_LIBUV=true EMILE_STATIC_LINK=true \
  ./project/scripts/run-sbt.sh "emile/testOnly *" "emile-fs2/testOnly *"
```

`project/scripts/run-sbt.sh` is the host-or-Docker sbt entry point. With `DOCKER_IMAGE` set, it runs sbt inside the
named image and forwards `EMILE_SYSTEM_LIBUV` / `EMILE_STATIC_LINK` into the container.

## Style and gates

- **Compiler flags.** `build.sbt`'s `compilerOptions` are binding and apply to both `Compile/compile` and
  `Test/compile`. `-Werror`, `-Yexplicit-nulls`, `-language:strictEquality`, `-Yrequire-targetName`,
  `-Ycheck-reentrant`, `-Wvalue-discard`, `-Wnonunit-statement`, and the `-Wunused:*` checks (implicits, explicits,
  imports, locals, params, privates).
- **scalafix.** `.scalafix.conf` enforces `DisableSyntax` (`noVars`, `noWhileLoops`, `noReturns`, `noNulls`, `noThrows`,
  `noAsInstanceOf`, ...). Justified FFI / hot-path deviations carry an in-source suppression
  (`// scalafix:off DisableSyntax` region or `// scalafix:ok` per-line) with a terse cost-justification comment.
- **scalafmt.** `.scalafmt.conf` is binding; `sbt scalafmtAll` is the formatter.
- **Headers.** Apache-2.0 file headers on every source file; `sbt headerCreateAll` keeps them in sync.

Run `sbt format` before pushing - it runs `scalafixAll`, `scalafmtAll`, `scalafmtSbt`, and `headerCreateAll` together;
an unsuppressed `DisableSyntax` lint or a compile failure fails the run.

## Documentation and comments

Documentation earns its place; it is not ceremony. For every item, of any visibility, the test is whether the
specific reader gains something the name and signature do not already convey. If not, write one line or nothing.

- **Scaladoc (`/** */`) documents an interface** - a public API for its users, or a package-internal API
  (`private[emile]` / `private[unsafe]`) consumed across files for its contributors. Keep it terse: state the
  behaviour, document a parameter only where its meaning is non-obvious, describe a type's representation, and
  link a companion with an alias (`[[Foo$ Foo]]`).
- **Bare-`private` implementation is not an interface** - it carries no Scaladoc. Where a step is non-obvious a
  single inline `//` explains *why* (it never restates *what* the code already says); where it is self-evident
  it carries nothing.
- **No decoration** - no section-divider banners (`// ==== ... ====`); the code's own structure organises the file.
- **Current state only** - comments and Scaladoc describe what the code does now, never planned or removed work,
  and never reference internal design notes; the only outward pointers are to published standards (e.g. RFC 7519)
  or this repository's own documentation.
- **Language** - ASCII only, UK English, and ` - ` (a spaced hyphen) rather than an em-dash.

## Contribution workflow

1. Branch off `main` (PRs coming from `main` will not be accepted).
2. Make your change. Add or extend tests where the change is behavioural.
3. Run `sbt format` to fix what can be fixed and surface lint failures.
4. Run the tests locally:
    - host with libuv 1.52+: `EMILE_SYSTEM_LIBUV=true sbt "emile/testOnly *" "emile-fs2/testOnly *"`
    - vendored fallback otherwise: `sbt "emile/testOnly *" "emile-fs2/testOnly *"`
    - concurrency or lifetime changes: also `EMILE_SYSTEM_LIBUV=true sbt "emile-stress/testOnly *"`
5. Push and open a PR. Ensure all CI jobs are green.

## Licence

Émile is licensed under the [Apache License, Version 2.0](LICENSE). By contributing you agree your contribution is
released under that licence.
