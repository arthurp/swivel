name := "swivel"
organization := "edu.utexas.cs"
version := "0.0.2"

scalaVersion := "2.12.2"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
