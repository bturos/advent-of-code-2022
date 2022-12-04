import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings

lazy val commonSettings = commonSmlBuildSettings ++ Seq(
  organization := "org.szklaniec",
  scalaVersion := "2.13.10"
)

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.14" % Test
val cats = "org.typelevel" %% "cats-core" % "2.9.0"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "advent-of-code-2022")
  .aggregate(puzzles)

lazy val puzzles: Project = (project in file("puzzles"))
  .settings(commonSettings: _*)
  .settings(
    name := "puzzles",
    libraryDependencies ++= Seq(
      cats,
      scalaTest
    )
  )
