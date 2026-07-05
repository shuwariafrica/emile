scalaVersion := "3.8.4"
organization := "africa.shuwari"
description := "Scala Native async I/O library backed by libuv."
startYear := Some(2025)
homepage := Some(url("https://github.com/shuwariafrica/emile"))
licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
semanticdbEnabled := true
scalafmtDetailedError := true
scalafmtPrintDiff := true
scmInfo := Some(
  ScmInfo(
    url("https://github.com/shuwariafrica/emile"),
    "scm:git:https://github.com/shuwariafrica/emile.git",
    Some("scm:git:git@github.com:shuwariafrica/emile.git")
  )
)

// Shuwari org POM defaults: organizationName, organizationHomepage, developers, versionScheme.
Shuwari.organisationSettings

val emile =
  project
    .in(file("modules/emile"))
    .enablePlugins(SNXPlugin, EmileNativeBuild)
    .settings(description := "Scala Native async I/O library backed by libuv, integrated with cats-effect.")
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += Dependencies.`cats-effect`)
    .settings(libraryDependencies += Dependencies.`cats-core`)
    .settings(libraryDependencies += Dependencies.`fs2-core`)
    .settings(libraryDependencies += Dependencies.`ip4s-core`)
    .settings(libraryDependencies += Dependencies.`boilerplate`)
    .settings(libraryDependencies += Dependencies.`boilerplate-effect`)
    .settings(libraryDependencies += Dependencies.`munit` % Test)
    .settings(libraryDependencies += Dependencies.`munit-cats-effect` % Test)
    .settings(nativeSettings)

val `emile-fs2` =
  project
    .in(file("modules/emile-fs2"))
    .enablePlugins(SNXPlugin, EmileNativeBuild)
    .dependsOn(emile)
    .settings(description := "fs2-networking interop for emile: TCPSocket/TCPServer adapters to fs2.io.net.Socket.")
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += Dependencies.`fs2-io`)
    .settings(libraryDependencies += Dependencies.`munit` % Test)
    .settings(libraryDependencies += Dependencies.`munit-cats-effect` % Test)
    .settings(nativeSettings)

// Concurrency stress + invariant suites, off the `emile-root` aggregate: run explicitly
// (`emile-stress/testOnly *`), not in the unit-test job; they force their own aggressive auto-cede runtime.
val `emile-stress` =
  project
    .in(file("modules/emile-stress"))
    .enablePlugins(SNXPlugin, EmileNativeBuild)
    .dependsOn(emile)
    .settings(description := "emile concurrency stress and invariant suites (off-aggregate; aggressive runtime).")
    .settings(commonSettings)
    .settings(publish / skip := true)
    .settings(libraryDependencies += Dependencies.`munit` % Test)
    .settings(libraryDependencies += Dependencies.`munit-cats-effect` % Test)
    .settings(nativeSettings)

val `emile-root` =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(emile, `emile-fs2`)

def commonSettings: Seq[Setting[?]] = Seq(
  // sbt-shuwari's ScalacOptions already carry almost all of emile's mandated set; add the two it omits, and
  // restore -Yexplicit-nulls / -language:strictEquality in tests (the org default excludes them from Test).
  Compile / compile / scalacOptions ++= Seq("-Ycheck-mods", "-Xcheck-macros"),
  Test / compile / scalacOptions ++= Seq("-Ycheck-mods", "-Xcheck-macros", "-Yexplicit-nulls", "-language:strictEquality"),
  Compile / doc / scalacOptions := Nil,
  Test / doc / scalacOptions := Nil,
  testFrameworks += new TestFramework("munit.Framework"),
  headerLicense := {
    val start = startYear.value.getOrElse(2025)
    val current = java.time.Year.now.getValue
    val years = if start == current then s"$current" else s"$start, $current"
    Some(HeaderLicense.ALv2(years, "Ali Rashid"))
  },
  headerEmptyLine := false,
  libraryDependencySchemes += "org.scala-native" % "test-interface_native0.5_3" % "always"
)

def nativeSettings: Seq[Setting[?]] = Seq(
  // links the test binary against system libuv (>= 1.51), and travels in the published NIR descriptor
  // so a downstream consumer links it too with no restatement.
  SNX.libraries := { case Linux(_, _) => Seq(NativeLibrary("uv")) },
  // emile drives a multithreaded cats-effect runtime: force multithreading on the test link and require it of consumers.
  SNX.flags := { case Linux(_, _) => Flags.multithreaded },
  // -no-pie defensively for us until the non-PIC relocation source issue is resolved.
  SNX.modifiers += Modifier.platform { case Linux(_, _) => _.linkOptions("-no-pie") },
  // A fully-static musl test binary when EMILE_STATIC_LINK is set - links the toolchain's static libuv archive.
  Test / SNX.modifiers += Modifier.platform { case Linux(_, Musl) if staticTestLink.value => _.linkOptions("-static") }
)

def publishSettings: Seq[Setting[?]] = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := (_ => false),
  publishTo := {
    if isSnapshot.value then Some("central-snapshots".at("https://central.sonatype.com/repository/maven-snapshots/"))
    else localStaging.value
  }
)

addCommandAlias("format", "scalafixAll; scalafmtAll; scalafmtSbt; headerCreateAll")
addCommandAlias("check", "scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck; headerCheckAll")
