val scalaNativeLibraryVersion = "0.5.10"
val scalaNativeVersion = settingKey[String]("Scala Native version")
scalaNativeVersion := scalaNativeLibraryVersion
enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](scalaNativeVersion)
buildInfoObject := "BuildInfo"

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeLibraryVersion)
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.2")

addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.6")
addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("no.arktekk.sbt" % "aether-deploy-signed" % "0.30.0")
// maven-resolver-supplier 1.9.23 (via aether-deploy) needs plexus-utils at runtime but doesn't declare it
// https://github.com/arktekk/sbt-aether-deploy/issues/43
dependencyOverrides += "org.codehaus.plexus" % "plexus-utils" % "3.6.0"
