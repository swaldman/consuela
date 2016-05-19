val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

organization := "com.mchange"

name := "consuela"

version := "0.0.2-SNAPSHOT"

scalaVersion := "2.11.4"

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


// NOTE: Updating to bouncycastle 1.53 breaks ethereum compatability for now, because the padding of the Keccak algorithm
//       was updated when it formally became SHA3, and so SHA3 hashing has changed.
//
//       We need to implement a new Keccak hash (AlgoName "KECCAK-256" I think, class name bouncycastle class name KeccakDigest)
//       and let EthHash refer to that rather than SHA3_256! We'll have to be careful whenever we use SHA/KECCAK hashes to be sure
//       we are using the right version. Grr. For now we're staying at 1.51 to keep our tests working.

libraryDependencies ++= Seq(
  "com.mchange" %% "mlog-scala" % "0.3.8",
  "com.mchange" %% "restricted-type" % "0.0.2-SNAPSHOT" changing(),
  "com.mchange" %% "leftright" % "0.0.1",
  "com.mchange" %% "mchange-commons-scala" % "0.4.1",
  "com.typesafe" % "config" % "1.2.1",
  "org.spire-math" %% "spire" % "0.9.1",
  //"org.bouncycastle" % "bcprov-jdk15on" % "1.51",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.54",
  "com.madgag.spongycastle" % "prov" % "1.54.0.0" % "compile,optional", //only necessary on android
  "com.mchange" %% "restricted-type-scalacheck-util" % "0.0.1" % "test",
  "org.specs2"  %% "specs2" % "2.4.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.7" % "test",        // for loading ethereum-test suites defined as json
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "test"
)

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


