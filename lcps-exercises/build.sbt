val scala3Version = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.7"
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
)
