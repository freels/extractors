import sbt._
import com.twitter.sbt.{StandardProject,TemplateProject}

class ExtractorProject(info: ProjectInfo) extends StandardProject(info) with TemplateProject {

  val jackson    = "org.codehaus.jackson" % "jackson-core-asl"   % "1.6.1" % "provided"
  val jacksonMap = "org.codehaus.jackson" % "jackson-mapper-asl" % "1.6.1" % "provided"

  val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6" % "test"
}
