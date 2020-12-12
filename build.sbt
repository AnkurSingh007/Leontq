name := "Leonteq"

version := "0.1"

scalaVersion := "2.13.4"

unmanagedClasspath in Compile += baseDirectory.value / "src/main/resource"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.14.0"

libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.0-RC1"
