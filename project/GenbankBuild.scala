import sbt._
import sbt.Keys._
import com.inthenow.sbt.scalajs._
import com.inthenow.sbt.scalajs.SbtScalajs._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import bintray.Plugin._
import org.eclipse.jgit.lib._

object GenbankBuild extends Build{
  val module = XModule(id = "genbank", defaultSettings = buildSettings)

  val logger = ConsoleLogger()

  val branch = fetchGitBranch()
  val baseVersion = "0.1.2"

  lazy val buildSettings: Seq[Setting[_]] = bintrayPublishSettings ++ Seq(
    resolvers += "bintray-alexander_myltsev" at "http://dl.bintray.com/alexander-myltsev/maven/",
    organization := "uk.co.turingatemyhamster",
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.11.4", "2.10.4"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    version := {
      if(branch == "main")
        baseVersion
      else
        s"$branch-$baseVersion"
    },
    publishMavenStyle := true,
    licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
  )

  lazy val genbank             = module.project(genbankPlatformJvm, genbankPlatformJs)
  lazy val genbankPlatformJvm  = module.jvmProject(genbankSharedJvm).
    settings(platformSettings : _*).settings(platformJvmSettings : _*)
  lazy val genbankPlatformJs   = module.jsProject(genbankSharedJs).
    settings(platformSettings : _*)
  lazy val genbankSharedJvm    = module.jvmShared().
    settings(sharedSettings : _*).settings(sharedJvmSettings : _*)
  lazy val genbankSharedJs     = module.jsShared(genbankSharedJvm).
    settings(sharedSettings : _*).settings(sharedJsSettings : _*)

  lazy val platformSettings = Seq(description := "Genbank platform-specific API")
  lazy val sharedSettings = Seq(description := "Genbank cross-platform API")

  lazy val platformJvmSettings = Seq(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
    )
  )

  lazy val sharedJvmSettings = Seq(
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.1"
  )

  lazy val sharedJsSettings = Seq(
    libraryDependencies += "org.parboiled" %%% "parboiled" % "2.0.1"
  )

  def fetchGitBranch(): String = {
    val builder = new RepositoryBuilder()
    builder.setGitDir(file(".git"))
    val repo = builder.readEnvironment().findGitDir().build()
    val branch = repo.getBranch
    repo.close()
    branch
  }
}
