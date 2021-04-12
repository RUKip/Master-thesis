name := "akka-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.1"

lazy val akkaVersion = "2.6.12"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "io.aeron" % "aeron-driver" % "1.32.0",
  "io.aeron" % "aeron-client" % "1.32.0",
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.choco-solver" % "choco-solver" % "4.10.6",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)