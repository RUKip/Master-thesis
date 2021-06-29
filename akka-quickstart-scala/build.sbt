import sbtassembly.AssemblyPlugin.assemblySettings
assemblySettings

name := "cop-solver"

version := "1.0"

scalaVersion := "2.13.1"

lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0-SNAPSHOT"
)

assembly / mainClass := Some("com.example.Main")

assemblyMergeStrategy in assembly := {
  case n if n.startsWith("reference.conf") => MergeStrategy.concat
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

lazy val akkaVersion = "2.6.12"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "io.aeron" % "aeron-driver" % "1.32.0",
  "io.aeron" % "aeron-client" % "1.32.0",
  "net.liftweb" %% "lift-json" % "3.4.3",
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.choco-solver" % "choco-solver" % "4.10.6",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)
