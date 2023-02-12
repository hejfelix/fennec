ThisBuild / organization := "fennec"
ThisBuild / scalaVersion := "3.3.0-RC2"
ThisBuild / scalacOptions ++= Seq("-source", "future")
ThisBuild / versionScheme := Some("early-semver")

val V = new {
  val calico     = "0.2.0-M4"
  val cats       = "2.9.0"
  val catsEffect = "3.4.5"
  val catsMtl    = "1.3.0"
  val circe      = "0.14.3"
  val fs2        = "3.5.0"
  val http4s     = "0.23.18"
  val scalajsDom = "2.3.0"
  val woof       = "0.4.7"
  val monocle    = "3.1.0"
  val weaver     = "0.8.1"
}

val commonSettings = Seq(
  scalacOptions --= Seq(
    "-encoding",
    "UTF-8"
  ), // we need this for IntelliJ to work w. TPolecat plugin,
  scalacOptions ++= Seq(
    "-Wunused:imports",
    "-Wunused:locals",
    "-Wunused:privates",
    "-Wunused:params",
    "-Wunused:unsafe-warn-patvars",
    "-Wunused:linted",
    "-Wunused:implicits"
  )
)

val testLibs = Seq(
  libraryDependencies += "com.disneystreaming" %%% "weaver-cats" % V.weaver % Test,
  testFrameworks += new TestFramework("weaver.framework.CatsEffect")
)

lazy val fennec = project
  .in(file("."))
  .aggregate(core.js, core.jvm, circeSupport.jvm, circeSupport.js)
  .settings(
    publish / skip := true,
    name           := "Fennec"
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "fennec-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"   % V.cats,
      "co.fs2"        %%% "fs2-core"    % V.fs2,
      "org.typelevel" %%% "cats-effect" % V.catsEffect,
      "org.legogroup" %%% "woof-core"   % V.woof,
      "org.typelevel" %%% "cats-mtl"    % V.catsMtl
    ),
    testLibs
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom"               % V.scalajsDom,
      ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13)
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % V.fs2
    )
  )

lazy val circeSupport = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/circe-support"))
  .settings(commonSettings)
  .settings(
    name := "fennec-circe-support",
    libraryDependencies ++=
      Seq("circe-core", "circe-parser")
        .map(m => "io.circe" %%% m % V.circe),
    testLibs
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .dependsOn(core)

lazy val serverHttp4s = project
  .in(file("modules/server-http4s"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-core"         % V.http4s,
      "org.http4s"    %% "http4s-dsl"          % V.http4s,
      "org.http4s"    %% "http4s-ember-server" % V.http4s,
      "org.legogroup" %% "woof-slf4j"          % V.woof
    )
  )
  .dependsOn(core.jvm)

lazy val examples = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/examples"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"      %%% "circe-generic" % V.circe,
      "org.legogroup" %%% "woof-core"     % V.woof,
      "dev.optics"    %%% "monocle-core"  % V.monocle,
      "dev.optics"    %%% "monocle-macro" % V.monocle
    ),
    testLibs
  )
  .dependsOn(
    core,
    circeSupport
  )
  .jvmConfigure(_.dependsOn(serverHttp4s))
  .jsConfigure(
    _.dependsOn(core.js)
      .settings(
        scalaJSUseMainModuleInitializer := true,
        scalaJSLinkerConfig ~= (_.withModuleKind(
          ModuleKind.CommonJSModule
        )) // configure Scala.js to emit a JavaScript module instead of a top-level script
      )
      .settings(
        libraryDependencies += "com.armanbilge" %%% "calico" % V.calico
      )
  )

addCommandAlias("watchJs", s"~; examplesJVM/reStart; examplesJS/fastOptJS;")
addCommandAlias(
  "dev",
  s"; examplesJVM/reStart; watchJs"
)
