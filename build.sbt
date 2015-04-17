val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

organization := "com.mchange"

name := "consuela"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

resolvers += ("releases" at nexusReleases)

resolvers += ("snapshots" at nexusSnapshots)

resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

publishTo <<= version {
  (v: String) => {
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexusSnapshots )
    else
      Some("releases"  at nexusReleases )
  }
}

libraryDependencies ++= Seq(
  "com.mchange" %% "mlog-scala" % "0.3.7-SNAPSHOT",
  "com.mchange" %% "mchange-commons-scala" % "0.4.0-SNAPSHOT" changing(),
  "com.typesafe" % "config" % "1.2.1",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.51",
  "com.madgag.spongycastle" % "prov" % "1.51.0.0" % "compile,optional", //only necessary on android
  "org.specs2"  %% "specs2" % "2.4.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.7" % "test",        // for loading ethereum-test suites defined as json
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "test"
)

