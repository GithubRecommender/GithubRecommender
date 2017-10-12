import sbt._

object Dependencies {

  val http4sVersion = "0.17.4"
  val circeVersion  = "0.8.0"

  val root = Seq(
    "org.http4s" %% "http4s-blaze-client" % http4sVersion % Compile,
    "org.http4s" %% "http4s-dsl" % http4sVersion          % Compile,
    "org.http4s" %% "http4s-circe" % http4sVersion        % Compile,
    "io.circe" %% "circe-core" % circeVersion    % Compile,
    "io.circe" %% "circe-parser" % circeVersion  % Compile,
    "io.circe" %% "circe-generic" % circeVersion % Compile,
    "eu.timepit" %% "refined" % "0.8.4" % Compile,

    "mysql" % "mysql-connector-java" % "6.0.6" % Compile,

    "ch.qos.logback" % "logback-classic" % "1.2.3" % Compile,
    "ch.qos.logback" % "logback-core" % "1.2.3"    % Compile,
    "org.slf4j" % "slf4j-api" % "1.7.25"           % Compile,

    "org.specs2"  %% "specs2-core" % "3.9.4" % "test;it"
  )
}
