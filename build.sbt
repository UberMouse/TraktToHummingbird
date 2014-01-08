scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.2.5",
  "org.scalaj" %% "scalaj-http" % "0.3.10",
  "org.streum" %% "configrity-core" % "1.0.0",
  "org.scalacheck" %% "scalacheck" % "1.10.1",
  "org.scalatest" %% "scalatest" % "2.0.RC1" % "test",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "org.slf4j" % "slf4j-api" % "1.7.1",
  "org.slf4j" % "log4j-over-slf4j" % "1.7.1",  // for any java classes looking for this
  "ch.qos.logback" % "logback-classic" % "1.0.3"
)

