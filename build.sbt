
lazy val commonSettings = Seq(
  organization  := "com.github.pheymann",
  version       := "0.1.0",
  scalaVersion  := "2.12.3",
  scalacOptions := Seq(
    "-deprecation",
    "-encoding", "utf-8",
    "-explaintypes",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:option-implicit",
    "-Xlint:type-parameter-shadow",
    "-Xlint:unsound-match",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:privates"
  )
)

lazy val root = project
  .in(file("."))
  .configs(IntegrationTest)
  .settings(
    commonSettings,
    Defaults.itSettings,
    libraryDependencies ++= Dependencies.root,
    ensimeScalaVersion in ThisBuild := scalaVersion.value,

    parallelExecution in IntegrationTest := false,

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
