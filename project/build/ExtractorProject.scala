import sbt._
import com.twitter.sbt.StandardProject

class ExtractorProject(info: ProjectInfo) extends DefaultProject(info) with TemplateProject {
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)

  val scalaToolsTesting  = "testing.scala-tools.org" at "http://scala-tools.org/repo-releases/testing/"

  val specs = "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test"
}
