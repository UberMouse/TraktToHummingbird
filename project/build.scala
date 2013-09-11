import sbt._
import Keys._

object ApplicationBuild extends Build {

  lazy val traktToHummingbird = Project(
    "TraktToHummingbird", file("."),
    settings = Defaults.defaultSettings ++ Seq(
      scalaVersion := "2.10.0",
      libraryDependencies ++= Seq(
        "org.json4s" %% "json4s-native" % "3.2.5",
        "org.scalaj" %% "scalaj-http" % "0.3.10"
      )
    )
  )
}