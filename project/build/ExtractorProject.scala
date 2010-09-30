import sbt._
import com.twitter.sbt.StandardProject

class ExtractorProject(info: ProjectInfo) extends DefaultProject(info) with TemplateProject {
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)

  val scalaTools  = "testing.scala-tools.org" at "http://scala-tools.org/repo-releases"

  val specs = buildScalaVersion match {
    case "2.8.0" => "org.scala-tools.testing" %% "specs" % "1.6.5" % "test"
    case "2.7.7" => "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test"
  }
}
