val scala3 = "3.3.1"

val commonSettings = Seq(
  organization := "dev.korban",
  description := "PoC library to turn Scala 3 functions into UI forms with a single line of code",
  homepage := Some(url("https://github.com/KacperFKorban/GUInep")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "KacperFKorban",
      "Kacper Korban",
      "kacper.f.korban@gmail.com",
      url("https://twitter.com/KacperKorban")
    )
  ),
  scalaVersion := scala3,
  // TODO(kπ) enable all macro checks
  scalacOptions ++= Seq(
    // "-Xcheck-macros",
    // "-Ycheck:inlining",
    "-explain",
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.0.0-M6" % Test
  )
)

lazy val root =
  project
    .in(file("."))
    .settings(
      name := "GUInep-root",
      publish / skip := true
    )
    .aggregate((guinep.projectRefs ++ swing.projectRefs): _*)

lazy val guinep = projectMatrix
  .in(file("guinep"))
  .settings(
    name := "GUInep",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "pprint" % "0.8.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
  .jvmPlatform(scalaVersions = List(scala3))

lazy val swing = projectMatrix
  .in(file("swing"))
  .settings(
    name := "GUInep-swing",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
  .dependsOn(guinep)
  .jvmPlatform(scalaVersions = List(scala3))
