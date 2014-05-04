import sbt._
import Keys._

object BuildSettings
{
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.0"
  val buildScalaVersion = "2.10.3"
  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object ChiselBuild extends Build {
  import BuildSettings._
  lazy val chisel = Project("chisel", file("chisel"))
  lazy val work = Project("work", file("."), settings = buildSettings) dependsOn(chisel)

}
