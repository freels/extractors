import sbt._
import com.twitter.sbt._

class ExtractorProject(info: ProjectInfo) extends StandardProject(info)
with TemplateProject
with DefaultRepos
with SubversionPublisher {

  val jackson    = "org.codehaus.jackson" % "jackson-core-asl"   % "1.7.3" % "provided"
  val jacksonMap = "org.codehaus.jackson" % "jackson-mapper-asl" % "1.7.3" % "provided"

  val util       = "com.twitter" % "util-core" % "1.11.1"

  val asm       = "asm"                     % "asm"         % "1.5.3" % "test"
  val cglib     = "cglib"                   % "cglib"       % "2.1_3" % "test"
  val objenesis = "org.objenesis"           % "objenesis"   % "1.1"   % "test"
  val specs     = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6" % "test"
  val jmock     = "org.jmock"               % "jmock"       % "2.4.0" % "test"

  override def pomExtra =
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>

  override def subversionRepository = Some("http://svn.local.twitter.com/maven-public")
}
