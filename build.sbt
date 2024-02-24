val zioVersion = "2.0.21"

lazy val compileDependencies = Seq(
  "dev.zio" %% "zio" % zioVersion
) map (_ % Compile)

lazy val settings = Seq(
  name := "zio-hangman",
  version := "2.0.0",
  scalaVersion := "2.13.12",
  libraryDependencies ++= compileDependencies
)

lazy val root = (project in file("."))
  .settings(settings)
