inThisBuild(
  List(
    scalaVersion := "3.7.4",
    organization := "io.github.arashi01",
    description := "",
    startYear := Some(2025),
    homepage := Some(url("https://github.com/arashi01/emile")),
    semanticdbEnabled := true,
    version := versionSetting.value,
    dynver := versionSetting.toTaskable.toTask.value,
    versionScheme := Some("semver-spec"),
    licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
    sonatypeCredentialHost := Sonatype.sonatypeCentralHost,
    publishCredentials,
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
  val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
  val `cats-effect` = Def.setting("org.typelevel" %%% "cats-effect" % "3.7.0-RC1")
  val fs2 = Def.setting("co.fs2" %%% "fs2-core" % "3.13.0-M7")
  val zio = Def.setting("dev.zio" %%% "zio" % "2.1.23")
  val `zio-streams` = zio.apply(_.withName("zio-streams"))
  
  // Testing
  val `munit-cats-effect` = Def.setting("org.typelevel" %%% "munit-cats-effect" % "2.2.0-RC1")
  val `munit-zio` = Def.setting("com.github.poslegm" %%% "munit-zio" % "0.4.0")
  val munit = Def.setting("org.scalameta" %%% "munit" % "1.2.1")
  val `scala-java-time` = Def.setting("io.github.cquiroz" %%% "scala-java-time" % "2.6.0")
}

// =============================================================================
// emile-ipa: Cross-platform IP address library (JVM, JS, Native)
// =============================================================================

lazy val `emile-ipa` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/ipa"))
  .settings(compilerSettings)
  .settings(unitTestSettings)
  .settings(fileHeaderSettings)
  .settings(publishSettings)
  .settings(
    name := "emile-ipa",
    description := "Cross-platform IP address and socket address types for Scala 3"
  )

lazy val `emile-ipa-jvm` = `emile-ipa`.jvm
lazy val `emile-ipa-js` = `emile-ipa`.js.enablePlugins(ScalaJSPlugin)
lazy val `emile-ipa-native` = `emile-ipa`.native.enablePlugins(ScalaNativePlugin)

val `emile-core` =
  project
    .in(file("modules/core"))
    .enablePlugins(ScalaNativePlugin)
    .dependsOn(`emile-ipa-native`)
    .settings(compilerSettings)
    .settings(unitTestSettings)
    .settings(fileHeaderSettings)
    .settings(publishSettings)


val `emile-cats` =
  project
    .in(file("modules/cats"))
    .enablePlugins(ScalaNativePlugin)
    .dependsOn(`emile-core`)
    .settings(compilerSettings)
    .settings(unitTestSettings)
    .settings(fileHeaderSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += libraries.cats.value)
    .settings(libraryDependencies += libraries.`cats-effect`.value)
    .settings(libraryDependencies += libraries.fs2.value)
		.settings(libraryDependencies += libraries.`scala-java-time`.value % Provided)
    .settings(libraryDependencies += libraries.`munit-cats-effect`.value % Test)


val `emile-zio` =
  project
    .in(file("modules/zio"))
    .enablePlugins(ScalaNativePlugin)
    .dependsOn(`emile-core`)
    .settings(compilerSettings)
    .settings(unitTestSettings)
    .settings(fileHeaderSettings)
    .settings(publishSettings)
    .settings(libraryDependencies += libraries.zio.value)
    .settings(libraryDependencies += libraries.`zio-streams`.value)
    .settings(libraryDependencies += libraries.`scala-java-time`.value % Provided)
    .settings(libraryDependencies += libraries.`munit-zio`.value % Test)



val `emile-root` =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(
      `emile-ipa-jvm`,
      `emile-ipa-js`,
      `emile-ipa-native`,
      `emile-core`,
      `emile-cats`,
      `emile-zio`,
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

  // Scala 3-specific checks
  "-Yrequire-targetName",
  "-Ycheck-reentrant",
  "-Ycheck-mods",
  "-Xcheck-macros",
)

def compilerOptions = baseCompilerOptions ++ List(
  "-Yexplicit-nulls",
  "-Xfatal-warnings"
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
    libraries.`munit-zio`.value % Test,
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
      Some(HeaderLicense.MIT(developmentTimeline, "the original author(s)."))
    },
    headerEmptyLine := false
  )

def publishCredentials: Setting[Task[Seq[Credentials]]] = credentials :=
  (for {
    user <- Option(System.getenv("PUBLISH_USER"))
    pass <- Option(System.getenv("PUBLISH_USER_PASSPHRASE"))
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    sonatypeCredentialHost.value,
    user,
    pass
  )).toSeq

def pgpSettings: List[Setting[?]] = List(
  PgpKeys.pgpSelectPassphrase := None,
  usePgpKeyHex(System.getenv("SIGNING_KEY_ID"))
)

def platformSourceDirectory(platform: String): Setting[Seq[File]] = sourceDirectories += (sourceDirectory.value / platform)

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

def publishSettings: List[Setting[?]] = publishCredentials +: pgpSettings ++: List(
  packageOptions += Package.ManifestAttributes(
    "Build-Jdk" -> System.getProperty("java.version"),
    "Specification-Title" -> name.value,
    "Specification-Version" -> Keys.version.value,
    "Implementation-Title" -> name.value
  ),
  publishTo := sonatypePublishToBundle.value,
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

addCommandAlias("format", "scalafixAll; scalafmtAll; scalafmtSbt; headerCreateAll")
addCommandAlias("staticCheck", "scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck; headerCheckAll")
