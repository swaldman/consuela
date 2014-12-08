name := "consuela"

version := "0.0.1"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

libraryDependencies ++= Seq(
  "com.mchange" %% "mlog-scala" % "0.3.6-SNAPSHOT" changing(),
  "com.mchange" %% "mchange-commons-scala" % "0.4.0-SNAPSHOT" changing(),
  "com.typesafe" % "config" % "1.2.1",
  "org.specs2"  %% "specs2" % "2.4.6" % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "test"
)






