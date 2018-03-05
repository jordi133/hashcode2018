name := "hashcode2018"
version := "0.0.1"

scalaVersion := "2.12.4"
lazy val scalaTestV = "3.0.1"

lazy val root = (project in file(".")).
  settings(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
  ),
    unmanagedClasspath in Test += baseDirectory.value / "input",
    unmanagedClasspath in (Compile, runMain) += baseDirectory.value / "input"
  )
