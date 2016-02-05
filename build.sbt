val genbank = crossProject.settings(
  organization := "uk.co.turingatemyhamster",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-deprecation", "-unchecked"),
  version := "0.1.3-SNAPSHOT",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1",
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.3.4",
  testFrameworks += new TestFramework("utest.runner.Framework")
).jvmSettings(xerial.sbt.Pack.packAutoSettings :_*)

lazy val js = genbank.js

lazy val jvm = genbank.jvm
