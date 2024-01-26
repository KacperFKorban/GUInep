val scala3Version = "3.3.1"

lazy val guinep = project
  .in(file("."))
  .settings(
    name := "guinep",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= Seq(
      // TODO(kπ) enable all macro checks
      // "-Xcheck-macros",
      // "-Xcheck:inlining"
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "com.lihaoyi" %% "pprint" % "0.8.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
