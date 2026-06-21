scalaVersion := "3.8.4"
organization := "io.github.arashi01"
description := "Scala Native async I/O library backed by libuv."
startYear := Some(2025)
homepage := Some(url("https://github.com/arashi01/emile"))
licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
versionScheme := Some("semver-spec")
semanticdbEnabled := true
scalafmtDetailedError := true
scalafmtPrintDiff := true
scmInfo := Some(
  ScmInfo(
    url("https://github.com/arashi01/emile"),
    "scm:git:https://github.com/arashi01/emile.git",
    Some("scm:git:git@github.com:arashi01/emile.git")
  )
)

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
    .settings(description := "fs2-networking interop for emile: TcpSocket/TcpServer adapters to fs2.io.net.Socket.")
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += Dependencies.`fs2-io`)
    .settings(libraryDependencies += Dependencies.`munit` % Test)
    .settings(libraryDependencies += Dependencies.`munit-cats-effect` % Test)
    .settings(nativeSettings)

val `emile-root` =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(emile, `emile-fs2`)

def compilerOptions: Seq[String] = Seq(
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:strictEquality",
  "-Xkind-projector",
  "-Xmax-inlines:64",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-explain",
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wunused:implicits",
  "-Wunused:explicits",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:params",
  "-Wunused:privates",
  "-Yrequire-targetName",
  "-Ycheck-reentrant",
  "-Ycheck-mods",
  "-Xcheck-macros",
  "-Yexplicit-nulls",
  "-Werror"
)

def commonSettings: Seq[Setting[?]] = Seq(
  Compile / compile / scalacOptions ++= compilerOptions,
  Test / compile / scalacOptions ++= compilerOptions,
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
  // travels in the published NIR descriptor so a downstream consumer links libuv with no restatement.
  SNX.libraries := { case Linux(_, _) => Seq(NativeLibrary("uv")) },
  SNX.libraries ++= (if useSystemLibUV.value then Seq.empty else Seq(Dependencies.vendoredLibUV % Test)),
  // emile drives a multithreaded cats-effect runtime: force multithreading on the test link and require it of consumers.
  SNX.flags := { case Linux(_, _) => Flags.multithreaded },
  // -no-pie defensively for us until the non-PIC relocation source issue is resolved.
  SNX.modifiers += Modifier.platform { case Linux(_, _) => _.linkOptions("-no-pie") },
  Test / SNX.modifiers += Modifier.platform { case Linux(_, Musl) if staticTestLink.value => _.linkOptions("-static") }
)

def publishSettings: Seq[Setting[?]] = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := (_ => false),
  publishTo := {
    if isSnapshot.value then Some("central-snapshots".at("https://central.sonatype.com/repository/maven-snapshots/"))
    else localStaging.value
  },
  developers := List(Developer("arashi01", "Ali Rashid", "https://github.com/arashi01", url("https://github.com/arashi01")))
)

addCommandAlias("format", "scalafixAll; scalafmtAll; scalafmtSbt; headerCreateAll")
addCommandAlias("check", "scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck; headerCheckAll")
