import sbt._

object Dependencies {

  val root = Seq(
    "org.scalaj" %% "scalaj-http" % "2.3.0"          % Compile,
    "org.http4s" %% "http4s-blaze-client" % "0.17.4" % Compile, 
    "com.typesafe.play" %% "play-json" % "2.6.2"     % Compile,

    "mysql" % "mysql-connector-java" % "6.0.6"     % Compile,

    "ch.qos.logback" % "logback-classic" % "1.2.3" % Compile,
    "ch.qos.logback" % "logback-core" % "1.2.3"    % Compile,
    "org.slf4j" % "slf4j-api" % "1.7.25"           % Compile,

    "org.specs2"  %% "specs2-core" % "3.9.4"     % "test;it"
  )
}
