name := "sbt-scala-tests"

version := "1.0"

scalaVersion := "2.11.1"
//enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2" % "test"

//scalacOptions ++= Seq("-deprecation", "-feature")
