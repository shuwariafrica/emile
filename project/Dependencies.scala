import sbt.*
import snx.sbt.SNXImports.*

object Dependencies:
  val `cats-effect` = "org.typelevel" %% "cats-effect" % "3.7.0"
  val `cats-core` = "org.typelevel" %% "cats-core" % "2.13.0"
  val `fs2-core` = "co.fs2" %% "fs2-core" % "3.13.0"
  val `fs2-io` = `fs2-core`.withName("fs2-io")
  val `ip4s-core` = "com.comcast" %% "ip4s-core" % "3.8.0"
  val `boilerplate` = "io.github.arashi01" %% "boilerplate" % "0.8.2"
  val `boilerplate-effect` = boilerplate.withName("boilerplate-effect")
  val `munit` = "org.scalameta" %% "munit" % "1.3.0"
  val `munit-cats-effect` = "org.typelevel" %% "munit-cats-effect" % "2.2.0"

  def vendoredLibUV = NativeLibrary(
    "uv",
    Vendored
      .git("https://github.com/libuv/libuv.git", "v1.52.1")
      .cmake(Seq("uv_a"), { case Linux(_, _) => Seq("-DLIBUV_BUILD_SHARED=OFF", "-DCMAKE_POSITION_INDEPENDENT_CODE=ON") })
      .options { case Linux(_, _) => Flags.libraries("pthread", "dl", "rt", "m") }
  )
end Dependencies
