name := "SpinalHDL_Extend"

version := "0.1"

scalaVersion := "2.11.12"

val spinalVersion = "1.8.0b"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
)