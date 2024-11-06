// See README.md for license details.

name := "stellar"

version := "3.1.0"
scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.4.+",
  "edu.berkeley.cs" %% "rocketchip" % "1.2.+",
  "edu.berkeley.cs" %% "chiseltest" % "0.5.0+" % "test",
  "com.github.simerplaha" %% "slack3d" % "0.1.0+",
  // "org.scalanlp" %% "breeze" % "1.1+",
)

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-language:postfixOps",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
  "-Xfatal-warnings",
)

fork in run := true
javaOptions in run ++= Seq(
  "-XstartOnFirstThread",
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal)

