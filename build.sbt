import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "Loris Zirah",
      scalaVersion := "2.12.7",
      version      := "1.0.0"
    )),
    name := "Battleship",
    libraryDependencies += scalaTest % Test
  )
