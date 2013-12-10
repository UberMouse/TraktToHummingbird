import sbt._
import Keys._

object ApplicationBuild extends Build {

  lazy val traktToHummingbirdAndMAL = Project(
    "TraktToHummingbirdAndMAL", file("."),
    settings = Defaults.defaultSettings ++ Seq(
      scalaVersion := "2.10.0",
      libraryDependencies ++= Seq(
        "org.json4s" %% "json4s-native" % "3.2.5",
        "org.scalaj" %% "scalaj-http" % "0.3.10",
        "org.streum" %% "configrity-core" % "1.0.0",
        "org.scalacheck" %% "scalacheck" % "1.10.1",
        "org.scalatest" %% "scalatest" % "2.0.RC1" % "test"
      )
    )
  )
}