ThisBuild / organization := "com.siriusxm"
ThisBuild / scalaVersion := "2.13.5"

lazy val root = (project in file(".")).settings(
  name := "shopping-cart-test-main",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    "com.squareup.okhttp3" % "okhttp" % "4.10.0",
    // Http Client
    "org.http4s" %% "http4s-ember-client" % "0.23.18",
    "org.http4s" %% "http4s-circe" % "0.23.18",
    // Json parser
    "io.circe" %% "circe-core" % "0.14.5",
    "io.circe" %% "circe-generic" % "0.14.5",
    "io.circe" %% "circe-refined" % "0.14.5",
    // Refined type
    "eu.timepit" %% "refined" % "0.9.28",
    // better monadic for compiler plugin as suggested by documentation
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

    // Test dependencies
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  ),
  scalacOptions += "-Ymacro-annotations"
)
