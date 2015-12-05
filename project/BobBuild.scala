import sbt._
import sbt.Keys._

object BobBuild extends Build {

  object Versions {
    val scalaz = "7.1.3"
    val scalazStream = "0.7.3a"
    val scalatest = "2.2.1"
  }

  lazy val bob3 = Project(
    id = "bob",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "bob",
      organization := "net.mcarolan",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.5",
      resolvers += "RichRelevance Bintray" at "http://dl.bintray.com/rr/releases",
      libraryDependencies += "org.scalaz" %% "scalaz-core" % Versions.scalaz,
      libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % Versions.scalaz,
      libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % Versions.scalazStream,
      libraryDependencies += "com.pi4j" % "pi4j-core" % "1.0",
      libraryDependencies += "org.scalatest" %% "scalatest" % Versions.scalatest % "test"
    )
  )
}
