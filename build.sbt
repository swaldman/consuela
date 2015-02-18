name := "consuela"

version := "0.0.1"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies ++= Seq(
  "com.mchange" %% "mlog-scala" % "0.3.6",
  "com.mchange" %% "mchange-commons-scala" % "0.4.0-SNAPSHOT" changing(),
  "com.typesafe" % "config" % "1.2.1",
  "org.specs2"  %% "specs2" % "2.4.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.7" % "test",        // for loading ethereum-test suites defined as json
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "test"
)






