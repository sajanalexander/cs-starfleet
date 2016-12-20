lazy val root = (project in file(".")).
  settings(
    name := "CS-Starfleet",
    version := "1.0",
    scalaVersion := "2.11.8",
    organization := "org.sajanalexander"
  )
  

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
