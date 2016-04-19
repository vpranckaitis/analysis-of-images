import sbt._

lazy val commonSettings = Seq(
  name := "image-analysis",
  organization := "lt.vpranckaitis",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

val boofCv = "org.boofcv" % "all" % "0.23"
val imgscalr = "org.imgscalr" % "imgscalr-lib" % "4.2"


val `image-analysis` = (project in file(".")).
  settings(commonSettings: _*).
  settings(libraryDependencies ++= boofCv :: imgscalr :: Nil)