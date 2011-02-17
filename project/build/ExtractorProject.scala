import sbt._
import com.twitter.sbt.{StandardProject,TemplateProject}

class ExtractorProject(info: ProjectInfo) extends StandardProject(info) with TemplateProject {

  val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6" % "test"
}
