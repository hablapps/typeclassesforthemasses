name := "workshop-lambda-2016"

organization := "org.hablapps"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds")

initialCommands in console := """
  | import org.hablapps.typeclasses._
  |""".stripMargin
