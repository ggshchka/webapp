//organization := "Your organization"
//name := "Webapp"
//version := "0.1.0"

scalaVersion := "2.13.1"
resolvers += "jitpack" at "https://jitpack.io"

enablePlugins(ScalaJSBundlerPlugin)

libraryDependencies ++= Seq(
  "com.github.outwatch.outwatch" %%% "outwatch" % "61deece8",
  "org.scalatest" %%% "scalatest" % "3.2.0" % Test,
  "com.github.cornerman.colibri" %%% "colibri-monix" % "9add104",
  "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
  "org.scalacheck" %%% "scalacheck" % "1.14.3",
)

npmDependencies in Compile ++= Seq(
  "jquery" -> "3.3",
  "bootstrap" -> "4.3",
)

useYarn := true // makes scalajs-bundler use yarn instead of npm
requireJsDomEnv in Test := true
scalaJSUseMainModuleInitializer := true
scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)) // configure Scala.webapp.js to emit a JavaScript module instead of a top-level script

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint" ::
  "-Xlint:adapted-args" ::
  "-Wextra-implicit" ::
  "-Xlint:infer-any" ::
  "-Wvalue-discard" ::
  "-Xlint:nullary-override" ::
  "-Xlint:nullary-unit" ::
  Nil

// hot reloading configuration:
// https://github.com/scalacenter/scalajs-bundler/issues/180
addCommandAlias("dev", "; compile; fastOptJS::startWebpackDevServer; devwatch; fastOptJS::stopWebpackDevServer")
addCommandAlias("devwatch", "~; fastOptJS; copyFastOptJS")

version in webpack := "4.43.0"
version in startWebpackDevServer := "3.11.0"
webpackDevServerExtraArgs := Seq("--progress", "--color")
webpackDevServerPort := 8080
webpackConfigFile in fastOptJS := Some(baseDirectory.value / "webpack.config.dev.js")

webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly() // https://scalacenter.github.io/scalajs-bundler/cookbook.html#performance

// when running the "dev" alias, after every fastOptJS compile all artifacts are copied into
// a folder which is served and watched by the webpack devserver.
// this is a workaround for: https://github.com/scalacenter/scalajs-bundler/issues/180
lazy val copyFastOptJS = TaskKey[Unit]("copyFastOptJS", "Copy javascript files to target directory")
copyFastOptJS := {
  val inDir = (crossTarget in (Compile, fastOptJS)).value
  val outDir = (crossTarget in (Compile, fastOptJS)).value / "dev"
  val files = Seq(name.value.toLowerCase + "-fastopt-loader.js", name.value.toLowerCase + "-fastopt.js") map { p => (inDir / p, outDir / p) }
  IO.copy(files, overwrite = true, preserveLastModified = true, preserveExecutable = true)
}
