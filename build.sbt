name := """play-scala-slick-example"""

version := "2.6.x"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.3"
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.mavenLocal
libraryDependencies += guice
libraryDependencies += "com.typesafe.play" %% "play-slick" %  "3.0.2"
libraryDependencies += "com.typesafe.play" %% "play-slick-evolutions" % "3.0.2"

libraryDependencies += "com.h2database" % "h2" % "1.4.194"
lazy val http4sVersion = "0.18.0-SNAPSHOT"
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-core" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "io.circe" %% "circe-parser" % "0.9.0-M3",
  "io.circe" %% "circe-generic" % "0.9.0-M3",
  "io.circe" %% "circe-generic-extras" % "0.9.0-M3",
  "io.circe" %% "circe-literal" % "0.9.0-M3",
  "org.scalaj" %% "scalaj-http" % "2.3.0",
  "com.github.pathikrit" %% "better-files" % "3.4.0",
  "com.github.andyglow" %% "websocket-scala-client" % "0.2.4" % Compile,
  "com.pubnub" % "pubnub-gson" % "4.16.0",
  "org.typelevel" %% "squants" % "1.3.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "com.lihaoyi" %% "sourcecode" % "0.1.4",
  "com.typesafe" % "config" % "1.3.1",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.beachape" %% "enumeratum" % "1.5.12",
  "org.scalatest" %% "scalatest" % "3.0.4" % Test
)


libraryDependencies += specs2 % Test
  

