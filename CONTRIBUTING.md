# Contributing to Émile

This document covers what you need to build Émile from source, run its tests, and submit changes.

## Prerequisites

- **JDK 17 or newer.** CI uses JDK 17; any modern JDK works locally.
- **An sbt launcher** (`sbt` on your `$PATH`). `project/build.properties` pins emile's sbt version; the launcher fetches
  it automatically. No system-wide sbt-2.x install is required.
- **Native toolchain.** `clang` (16 or newer recommended) and `lld`. Émile is Linux-only.
- **libuv >= 1.51, with its development package.** emile's `@extern` bindings link against the system libuv, so you
  need the linkable library (the `-dev` / `-devel` package), not just the runtime: `apt install libuv1-dev`,
  `dnf install libuv-devel`, or `apk add libuv-dev`. RHEL 10, Ubuntu 26.04, Alpine 3.23, and Fedora all ship a new-enough
  libuv.

## Running tests

Execute tests linked against your local libuv:

```bash
sbt "emile/testOnly *" "emile-fs2/testOnly *"
```

### Concurrency stress suite

`emile-stress` holds the concurrency-invariant suites (e.g. `AffinitySpec`), off the default aggregate and run
explicitly. They build their own forced aggressive auto-cede runtime so scheduler races surface (the default
thresholds hide them). CI runs them on every change (the `stress` job); locally:

```bash
sbt "emile-stress/testOnly *"
```

### Reproducing a CI cell verbatim with Docker

```bash
# glibc (Oracle Linux 10 = the RHEL 10 libuv 1.51 floor):
DOCKER_IMAGE=shuwariafrica/el10-jdk:17 \
  ./project/scripts/run-sbt.sh "emile/testOnly *" "emile-fs2/testOnly *"

# musl, fully-static (EMILE_STATIC_LINK adds -static, linking the image's libuv-static archive):
DOCKER_IMAGE=shuwariafrica/alpine-jdk:17 EMILE_STATIC_LINK=true \
  ./project/scripts/run-sbt.sh "emile/testOnly *" "emile-fs2/testOnly *"
```

`project/scripts/run-sbt.sh` is the host-or-Docker sbt entry point. With `DOCKER_IMAGE` set, it runs sbt inside the
named image (which ships libuv): the glibc cells use `shuwariafrica/el10-jdk:17` (libuv 1.51) and the musl cells
`shuwariafrica/alpine-jdk:17` (Alpine 3.23, ships `libuv-static`). `EMILE_STATIC_LINK=true` is honoured only on musl and
is best-effort until upstream Scala Native musl support ships (see the [musl caveat](README.md#caveats)).

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

## Contribution workflow

1. Branch off `main` (PRs coming from `main` will not be accepted).
2. Make your change. Add or extend tests where the change is behavioural.
3. Run `sbt format` to fix what can be fixed and surface lint failures.
4. Run the tests locally:
    - `sbt "emile/testOnly *" "emile-fs2/testOnly *"`
    - concurrency or lifetime changes: also `sbt "emile-stress/testOnly *"`
5. Push and open a PR. Ensure all CI jobs are green.

## Licence

Émile is licensed under the [Apache License, Version 2.0](LICENSE). By contributing you agree your contribution is
released under that licence.

