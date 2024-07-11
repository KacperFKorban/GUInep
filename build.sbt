val scala3 = "3.3.3"

val commonSettings = Seq(
  organization := "io.github.kacperfkorban",
  description := "Automatic UI forms for Scala 3 functions",
  homepage := Some(url("https://github.com/KacperFKorban/GUInep")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "KacperFKorban",
      "Kacper Korban",
      "kacper.f.korban@gmail.com",
      url("https://twitter.com/KacperFKorban")
    )
  ),
  scalaVersion := scala3,
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

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "GUInep-root",
    publish / skip := true
  )
  .aggregate((guinep.projectRefs ++ web.projectRefs ++ testcases.projectRefs): _*)

lazy val guinep = projectMatrix
  .in(file("guinep"))
  .settings(commonSettings)
  .settings(
    name := "GUInep",
    libraryDependencies ++= Seq(
      "com.softwaremill.quicklens" %%% "quicklens" % "1.9.7"
    )
  )
  .jvmPlatform(scalaVersions = List(scala3))

lazy val web = projectMatrix
  .in(file("web"))
  .settings(commonSettings)
  .settings(
    name := "GUInep-web",
    Compile / doc / scalacOptions ++= Seq(
      "-siteroot", "docs"
    ),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % "3.0.0-RC4",
      "dev.zio" %% "zio-json" % "0.6.2"
    )
  )
  .dependsOn(guinep)
  .jvmPlatform(scalaVersions = List(scala3))

lazy val testcases = projectMatrix
  .in(file("testcases"))
  .settings(commonSettings)
  .settings(
    name := "GUInep-testcases",
    publish / skip := true
  )
  .dependsOn(web)
  .jvmPlatform(scalaVersions = List(scala3))
