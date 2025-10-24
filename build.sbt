name := "Twinlight"
ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / organization := "org.example"

val spinalVersion = "1.12.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val projectname = (project in file("."))
  .settings(
    Compile / scalaSource := baseDirectory.value / "rtl" / "main" / "scala" ,
    libraryDependencies ++= Seq(spinalCore,
      spinalLib,
      spinalIdslPlugin,
      "org.scalatest" %% "scalatest" % "3.2.17"
    )
  )

fork := true
