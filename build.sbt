inThisBuild(
  List(
    scalaVersion := "3.8.2",
    organization := "io.github.arashi01",
    description := "Scala Native async I/O library backed by libuv.",
    startYear := Some(2025),
    homepage := Some(url("https://github.com/arashi01/emile")),
    semanticdbEnabled := true,
    version := versionSetting.value,
    dynver := versionSetting.toTaskable.toTask.value,
    versionScheme := Some("semver-spec"),
    licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/arashi01/emile"),
        "scm:git:https://github.com/arashi01/emile.git",
        Some("scm:git:git@github.com:arashi01/emile.git")
      )
    )
  ) ++ formattingSettings
)

val libraries = new {
  val boilerplate = Def.setting("io.github.arashi01" %%% "boilerplate" % "0.7.0")
  val `boilerplate-effect` = boilerplate(_.withName("boilerplate-effect"))
  val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
  val `cats-effect` = Def.setting("org.typelevel" %%% "cats-effect" % "3.7.0")
  val fs2 = Def.setting("co.fs2" %%% "fs2-core" % "3.13.0")

  // Testing
  val `munit-cats-effect` = Def.setting("org.typelevel" %%% "munit-cats-effect" % "2.2.0")
  val munit = Def.setting("org.scalameta" %%% "munit" % "1.2.4")
  val `scala-java-time` = Def.setting("io.github.cquiroz" %%% "scala-java-time" % "2.6.0")
}

lazy val `emile-ipa` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(NativePlatform)
  .in(file("modules/ipa"))
  .settings(compilerSettings)
  .settings(unitTestSettings)
  .settings(fileHeaderSettings)
  .settings(publishSettings)
  .settings(libraryDependencies += libraries.boilerplate.value)
  .settings(description := "Cross-platform IP address and socket address types for Scala 3.")

val `emile-core` =
  project
    .in(file("modules/core"))
    .enablePlugins(ScalaNativePlugin)
    .dependsOn(`emile-ipa`.native)
    .settings(description := "Scala Native libuv bindings and handle abstractions.")
    .settings(compilerSettings)
    .settings(unitTestSettings)
    .settings(fileHeaderSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += libraries.boilerplate.value)
    .settings(Test / parallelExecution := false)

val `emile-cats` =
  project
    .in(file("modules/cats"))
    .enablePlugins(ScalaNativePlugin)
    .dependsOn(`emile-ipa`.native)
    .dependsOn(`emile-core`)
    .settings(description := "Scala Native cats-effect async I/O runtime backed by libuv.")
    .settings(compilerSettings)
    .settings(unitTestSettings)
    .settings(fileHeaderSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += libraries.cats.value)
    .settings(libraryDependencies += libraries.`cats-effect`.value)
    .settings(libraryDependencies += libraries.`boilerplate-effect`.value)
    .settings(libraryDependencies += libraries.`scala-java-time`.value % Test)
    .settings(libraryDependencies += libraries.`munit-cats-effect`.value % Test)

val `emile-native` =
  project
    .in(file(".aggregate/native"))
    .settings(publish / skip := true)
    .aggregate(
      `emile-ipa`.native,
      `emile-core`,
      `emile-cats`
    )

val `emile-jvm` =
  project
    .in(file(".aggregate/jvm"))
    .settings(publish / skip := true)
    .aggregate(
      `emile-ipa`.jvm
    )

val `emile-js` =
  project
    .in(file(".aggregate/js"))
    .settings(publish / skip := true)
    .aggregate(
      `emile-ipa`.js
    )

val `emile-root` =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(
      `emile-native`,
      `emile-jvm`,
      `emile-js`
    )

def baseCompilerOptions = List(
  // Language features
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:strictEquality",

  // Kind projector / macros
  "-Xkind-projector",
  "-Xmax-inlines:64",

  // Core checks
  "-unchecked",
  "-deprecation",
  "-feature",
  "-explain",

  // Warning flags
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wunused:implicits",
  "-Wunused:explicits",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:params",
  "-Wunused:privates",
  "-Werror",

  // Scala 3-specific checks
  "-Yrequire-targetName",
  "-Ycheck-reentrant",
  "-Ycheck-mods",
)

def compilerOptions = baseCompilerOptions ++ List(
  "-Yexplicit-nulls",
  "-Xcheck-macros",
)

def compilerSettings = List(
  Compile / compile / scalacOptions ++= compilerOptions,
  Test / compile / scalacOptions ++= baseCompilerOptions,
  Compile / doc / scalacOptions := Nil,
  Test / doc / scalacOptions := Nil
)

def formattingSettings = List(
  scalafmtDetailedError := true,
  scalafmtPrintDiff := true
)

def unitTestSettings: List[Setting[?]] = List(
  libraryDependencies ++= List(
    libraries.munit.value % Test
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

def fileHeaderSettings: List[Setting[?]] =
  List(
    headerLicense := {
      val developmentTimeline = {
        import java.time.Year
        val start = startYear.value.get
        val current: Int = Year.now.getValue
        if (start == current) s"$current" else s"$start, $current"
      }
      Some(HeaderLicense.ALv2(developmentTimeline, "Ali Rashid."))
    },
    headerEmptyLine := false
  )

def pgpSettings: List[Setting[?]] = List(
  PgpKeys.pgpSelectPassphrase := None,
  usePgpKeyHex(System.getenv("SIGNING_KEY_ID"))
)

def versionSetting: Def.Initialize[String] = Def.setting(
  dynverGitDescribeOutput.value.mkVersion(
    (in: sbtdynver.GitDescribeOutput) =>
      if (!in.isSnapshot()) in.ref.dropPrefix
      else {
        val ref = in.ref.dropPrefix
        // Strip pre-release or build metadata (e.g., "-m.1" or "+build.5")
        val base = ref.takeWhile(c => c != '-' && c != '+')
        val numericParts =
          base.split("\\.").toList.map(_.trim).flatMap(s => scala.util.Try(s.toInt).toOption)

        if (numericParts.nonEmpty) {
          val incremented = numericParts.updated(numericParts.length - 1, numericParts.last + 1)
          s"${incremented.mkString(".")}-SNAPSHOT"
        } else {
          s"$base-SNAPSHOT"
        }
      },
    "SNAPSHOT"
  )
)

def publishSettings: List[Setting[?]] = pgpSettings ++: aetherSettings ++: List(
  packageOptions += Package.ManifestAttributes(
    "Build-Jdk" -> System.getProperty("java.version"),
    "Specification-Title" -> name.value,
    "Specification-Version" -> Keys.version.value,
    "Implementation-Title" -> name.value
  ),
  publishTo := {
    if (Keys.version.value.toLowerCase.contains("snapshot"))
      Some("central-snapshots".at("https://central.sonatype.com/repository/maven-snapshots/"))
    else localStaging.value
  },
  pomIncludeRepository := (_ => false),
  publishMavenStyle := true,
  developers := List(
    Developer(
      "arashi01",
      "Ali Rashid",
      "https://github.com/arashi01",
      url("https://github.com/arashi01")
    )
  )
)

// Maven-native snapshot deployment via sbt-aether-deploy (timestamped SNAPSHOTs with maven-metadata.xml)
def aetherSettings: List[Setting[?]] = List(
  credentials ++= {
    val user = sys.env.get("SONATYPE_USERNAME")
    val pass = sys.env.get("SONATYPE_PASSWORD")
    (user, pass) match {
      case (Some(u), Some(p)) => List(Credentials("central-snapshots", "central.sonatype.com", u, p))
      case _                  => Nil
    }
  }
)

addCommandAlias("format", "scalafixAll; scalafmtAll; scalafmtSbt; headerCreateAll")
addCommandAlias("check", "scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck; headerCheckAll")
