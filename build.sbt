name := "CheeseEngine"

version := "1.0"

scalaVersion := "2.10.4"

mainClass in (Compile, run) := Some("Main")

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.3.0"