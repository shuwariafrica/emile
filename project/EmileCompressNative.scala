import sbt.*

import scala.sys.process.Process
import scala.sys.process.ProcessLogger

import snx.sbt.SNXImports.*

/** emile-compress native provisioning recipes: the vendored codec archives, built from source at
  * pinned release tags and folded into the binding tests' link. Each module exports a bare
  * `NativeLibrary(name)` in its NIR descriptor and provisions the matching recipe here for its own
  * tests; a downstream consumer provisions it once with
  * `SNX.libraries += EmileCompressNative.<name>`.
  */
object EmileCompressNative:

  /** zlib-ng, the native (non-compat) DEFLATE codec. `ZLIB_COMPAT=OFF` keeps the `zng_`-prefixed
    * API, whose symbols do not collide with the system libz that scala-native's `java.util.zip`
    * links; tests are disabled so the build needs no gtest. Runtime CPU detection stays on (the
    * default), so one archive dispatches SIMD by CPUID. Named `z-ng` after the `libz-ng.a` it
    * emits.
    */
  val zlibNg: NativeLibrary =
    NativeLibrary(
      "z-ng",
      Vendored
        .git("https://github.com/zlib-ng/zlib-ng.git", "2.3.3")
        .cmake(
          Seq.empty[String],
          { case _ => Seq("-DZLIB_COMPAT=OFF", "-DZLIB_ENABLE_TESTS=OFF", "-DZLIBNG_ENABLE_TESTS=OFF", "-DWITH_GTEST=OFF") }
        )
    )

  /** The brotli release emile-compress is verified against. */
  val brotliTag: String = "v1.2.0"

  private val brotliRepository: String = "https://github.com/google/brotli.git"

  /** Build brotli's three static archives with CMake, collecting them from the build tree directly.
    *
    * The CMake backend cannot drive brotli: its `install()` writes man pages to an absolute system
    * man path (ignoring the staging `--prefix`), which fails on a read-only prefix. This command
    * build configures and builds the three archive targets, then returns them enc, dec, common -
    * common last, since both the encoder and decoder reference it and a static archive resolves
    * only the symbols the archives before it left undefined. The order is the whole point of a
    * command build here: the CMake backend collects install output in an unspecified order.
    */
  private def buildBrotli(context: BuildContext): Artefacts =
    val buildDir = context.staging / "build"
    val jobs = math.max(1, java.lang.Runtime.getRuntime.availableProcessors)
    val configure = Seq(
      "cmake",
      "-S",
      context.source.getAbsolutePath,
      "-B",
      buildDir.getAbsolutePath,
      "-DCMAKE_BUILD_TYPE=Release",
      "-DBUILD_SHARED_LIBS=OFF",
      s"-DCMAKE_C_COMPILER=${context.clang.getAbsolutePath}",
      "-DBROTLI_DISABLE_TESTS=ON",
      "-DBROTLI_BUILD_TOOLS=OFF"
    )
    val build = Seq("cmake", "--build", buildDir.getAbsolutePath, "--parallel", jobs.toString) ++
      Seq("brotlienc", "brotlidec", "brotlicommon").flatMap(target => Seq("--target", target))
    val logger = ProcessLogger(line => context.log.info(line), line => context.log.error(line))
    context.log.info(configure.mkString("snx brotli: ", " ", ""))
    if Process(configure).!(logger) != 0 then sys.error(s"snx: brotli configure failed: ${configure.mkString(" ")}")
    if Process(build).!(logger) != 0 then sys.error(s"snx: brotli build failed: ${build.mkString(" ")}")
    val archives = Seq(buildDir / "libbrotlienc.a", buildDir / "libbrotlidec.a", buildDir / "libbrotlicommon.a")
    archives.foreach(archive => if !archive.isFile then sys.error(s"snx: brotli build produced no archive at ${archive.getAbsolutePath}"))
    // No include dirs: the bindings are @extern (headers are re-declared in Scala), and a backend's
    // outputs must live under the staging directory.
    Artefacts(archives, Seq.empty[File])
  end buildBrotli

  /** brotli, built from source at a pinned tag and folded into the link (enc, dec, common). Named
    * `brotli`.
    */
  val brotli: NativeLibrary =
    NativeLibrary(
      "brotli",
      Vendored
        .git(brotliRepository, brotliTag)
        // The token is the whole cache key for a `command` build, so it MUST change whenever
        // buildBrotli changes, or a stale archive set is reused.
        .command("brotli-static-enc-dec-common-1")(buildBrotli)
    )

  /** The zstd release emile-compress is verified against. */
  val zstdTag: String = "v1.5.7"

  private val zstdRepository: String = "https://github.com/facebook/zstd.git"

  /** Build libzstd as a static archive with the reference `lib` Makefile.
    *
    * zstd's CMake lives under `build/cmake`, not the clone root, so the CMake backend (which
    * requires a root `CMakeLists.txt`) cannot drive it; the `lib` Makefile is the supported
    * source-list build. It is single-threaded by construction: `ZSTD_MULTITHREAD` is not defined,
    * so the archive carries no pthread dependency and `nbWorkers` stays 0 - caller-side parallelism
    * composes instead. sbt-snx requires a backend to write under the context staging directory, so
    * the source is copied there and built out of the cached clone.
    */
  private def buildZstd(context: BuildContext): Artefacts =
    val buildDir = context.staging / "build"
    IO.copyDirectory(context.source, buildDir)
    val libDir = buildDir / "lib"
    val jobs = math.max(1, java.lang.Runtime.getRuntime.availableProcessors)
    val command = Seq(
      "make",
      "-C",
      libDir.getAbsolutePath,
      "libzstd.a",
      s"CC=${context.clang.getAbsolutePath}",
      s"-j$jobs"
    )
    context.log.info(command.mkString("snx zstd: ", " ", ""))
    val logger = ProcessLogger(line => context.log.info(line), line => context.log.error(line))
    if Process(command).!(logger) != 0 then sys.error(s"snx: libzstd build failed: ${command.mkString(" ")}")
    val archive = libDir / "libzstd.a"
    if !archive.isFile then sys.error(s"snx: libzstd build produced no archive at ${archive.getAbsolutePath}")
    Artefacts(Seq(archive), Seq(libDir))
  end buildZstd

  /** zstd, built from source at a pinned tag and folded into the link. Named `zstd` after
    * `libzstd.a`.
    */
  val zstd: NativeLibrary =
    NativeLibrary(
      "zstd",
      Vendored
        .git(zstdRepository, zstdTag)
        // The token is the whole cache key for a `command` build (the build function is not hashed),
        // so it MUST change whenever buildZstd changes, or a stale archive is reused.
        .command("libzstd-static-singlethread-1")(buildZstd)
    )
end EmileCompressNative
