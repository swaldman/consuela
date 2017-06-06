val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

organization := "com.mchange"

name := "consuela"

version := "0.0.3-SNAPSHOT"

scalaVersion := "2.11.11"

// annoyingly, tests have 2.11 dependencies for now, so they won't run
// unless crossScalaVersions is commented out
crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2")

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked" /*, "-Xlog-implicits" */)

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

val restrictedTypeVersion = "0.0.3-SNAPSHOT"


libraryDependencies ++= Seq(
  "com.mchange"             %% "mlog-scala"                      % "0.3.10-SNAPSHOT",
  "com.mchange"             %% "restricted-type"                 % restrictedTypeVersion,
  "com.mchange"             %% "yinyang"                         % "0.0.2-SNAPSHOT",
  "com.mchange"             %% "mchange-commons-scala"           % "0.4.3-SNAPSHOT",
  "com.mchange"             %% "literal"                         % "0.0.2-SNAPSHOT" changing(),
  "com.mchange"             %% "mchange-play-json-util"          % "0.0.1-SNAPSHOT" changing(),
  "com.typesafe"            % "config"                           % "1.2.1",
  "org.bouncycastle"        % "bcprov-jdk15on"                   % "1.54",
  "com.madgag.spongycastle" % "prov"                             % "1.54.0.0"            % "compile,optional", //only necessary on android
  "com.mchange"             %% "restricted-type-scalacheck-util" % restrictedTypeVersion % "test",
  "org.specs2"              %% "specs2"                          % "2.4.17"              % "test",
  "ch.qos.logback"          % "logback-classic"                  % "1.1.2"               % "test"
)

libraryDependencies += {
  CrossVersion.partialVersion(Keys.scalaVersion.value) match {
    case Some((2, 12)) => {
      "com.typesafe.play" %% "play-json" % "2.6.0-RC2"
    }
    case Some((2, 11)) => {
      "com.typesafe.play" %% "play-json" % "2.5.15"
    }
    case _ => {
      "com.typesafe.play" %% "play-json" % "2.4.9"
    }
  }
}

fork in (Test, run) := true

javaOptions := Seq("-d64", "-Xms4g",  "-Xmx4g", "-verbose:gc", "-XX:+UnlockCommercialFeatures", "-XX:+FlightRecorder" )

pomExtra <<= name {
  (projectName : String ) => (
    <url>https://github.com/swaldman/{projectName}</url>
    <licenses>
      <license>
        <name>GNU Lesser General Public License, Version 2.1</name>
        <url>http://www.gnu.org/licenses/lgpl-2.1.html</url>
        <distribution>repo</distribution>
      </license>
      <license>
        <name>Eclipse Public License, Version 1.0</name>
        <url>http://www.eclipse.org/org/documents/epl-v10.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:swaldman/{projectName}.git</url>
      <connection>scm:git:git@github.com:swaldman/{projectName}</connection>
    </scm>
    <developers>
      <developer>
        <id>swaldman</id>
        <name>Steve Waldman</name>
        <email>swaldman@mchange.com</email>
      </developer>
    </developers>
  )
}


