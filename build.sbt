val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

organization := "com.mchange"

name := "consuela"

version := "0.3.0-SNAPSHOT"

scalaVersion := "2.12.10"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.10")

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked" /*, "-Xlog-implicits" */)

resolvers += ("releases" at nexusReleases)

resolvers += ("snapshots" at nexusSnapshots)

resolvers += ("Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases")

resolvers += ("Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/")

publishTo := {
  val v = version.value
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexusSnapshots )
  }
  else {
    Some("releases"  at nexusReleases )
  }
}

val restrictedTypeVersion = "0.0.6"
val failableVersion       = "0.0.5-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.mchange"             %% "mlog-scala"                      % "0.3.13",
  "com.mchange"             %% "failable"                        % failableVersion        changing(),
  "com.mchange"             %% "failable-logging"                % failableVersion        changing(),
  "com.mchange"             %% "restricted-type"                 % restrictedTypeVersion,
  "com.mchange"             %% "yinyang"                         % "0.0.2",
  "com.mchange"             %% "mchange-commons-scala"           % "0.4.12",
  "com.mchange"             %% "literal"                         % "0.1.0",
  "com.mchange"             %% "mchange-play-json-util"          % "0.0.4",
  "com.mchange"             %% "jsonrpc-client"                  % "0.0.8-SNAPSHOT" changing(),
  "org.reactivestreams"     %  "reactive-streams"                % "1.0.1",
  "com.typesafe"            %  "config"                          % "1.3.0",
  "org.bouncycastle"        %  "bcprov-jdk15on"                  % "1.54",
  "com.madgag.spongycastle" %  "prov"                            % "1.54.0.0"            % "compile,optional", //only necessary on android
  "com.mchange"             %% "restricted-type-scalacheck-util" % restrictedTypeVersion % "test",
  "org.specs2"              %% "specs2-core"                     % "3.10.0"              % "test",
  "org.scalacheck"          %% "scalacheck"                      % "1.14.1"              % "test",
  "ch.qos.logback"          % "logback-classic"                  % "1.1.2"               % "test",
)

libraryDependencies ++= {
  CrossVersion.partialVersion(Keys.scalaVersion.value) match {
    case Some((2, 12)) => Seq(
      "com.typesafe.play" %% "play-json"  % "2.6.13"
    )
    case Some((2, 11)) => Seq( 
      "com.typesafe.play" %% "play-json" % "2.5.18"
    )
    case _ => Seq (
      "com.typesafe.play" %% "play-json" % "2.4.11"
    )
  }
}

fork in (Test, run) := true

javaOptions := Seq("-d64", "-Xms4g",  "-Xmx4g", "-verbose:gc", "-XX:+UnlockCommercialFeatures", "-XX:+FlightRecorder" )

pomExtra := {
    <url>https://github.com/swaldman/{name.value}</url>
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
      <url>git@github.com:swaldman/{name.value}.git</url>
      <connection>scm:git:git@github.com:swaldman/{name.value}</connection>
    </scm>
    <developers>
      <developer>
        <id>swaldman</id>
        <name>Steve Waldman</name>
        <email>swaldman@mchange.com</email>
      </developer>
    </developers>
}

enablePlugins(ParadoxPlugin)

val updateSite = taskKey[Unit]("Updates the project website on tickle")

updateSite := {
  import scala.sys.process._

  val dummy1 = (Compile / paradox).value // force a build of the site

  val localDir1 = target.value / "paradox" / "site" / "main"

  val local1 = localDir1.listFiles.map( _.getPath ).mkString(" ")
  val remote1 = s"tickle.mchange.com:/home/web/public/www.mchange.com/projects/${name.value}-versions/${version.value}/"
  s"rsync -avz ${local1} ${remote1}"!

  val dummy2 = (Compile / doc).value // force scaladocs

  val localDir2 = target.value / "scala-2.12" / "api"
  val local2 = localDir2.listFiles.map( _.getPath ).mkString(" ")
  val remote2 = s"tickle.mchange.com:/home/web/public/www.mchange.com/projects/${name.value}-versions/${version.value}/apidocs"
  s"rsync -avz ${local2} ${remote2}"!
}
