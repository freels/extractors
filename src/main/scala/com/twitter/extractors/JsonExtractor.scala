package com.twitter.extractors
package json

import scala.collection.generic.CanBuild
import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.JsonParseException
import com.twitter.extractors.exceptions._


class JsonRoot(val root: JsonNode)

object JsonRoot {
  val mapper = new ObjectMapper

  protected def parse(s: String) = mapper.readValue(s, classOf[JsonNode])

  implicit def string2Json(s: String) = new JsonRoot(parse(s))
  implicit def bytes2Json(b: Array[Byte]) = new JsonRoot(parse(new String(b, "UTF-8")))
  implicit def parsed2Json(p: JsonNode) = new JsonRoot(p)
}

object JsonObjectExtractor extends ExtractorFactory with KeyedExtractors with IterableExtractors {
  type Container = JsonRoot
  type Key = String

  def KeyExtractor[R](key: Key, inner: Extractor[R]) = new SubcontainerExtractor[R](inner) {
    def subcontainer(c: Container) = c.root.get(key) match {
      case null => None
      case n    => Some(new JsonRoot(n))
    }
  }

  def IterableExtractor[R, CC[R]](inner: Extractor[R], bf: CanBuild[R,CC[R]]): Extractor[CC[R]] = {
    new IterableExtractor[R,CC](inner, bf) {
      def subcontainers(c: Container) = c.root match {
        case c if c.isArray => c.getElements map { new JsonRoot(_) } toIterable
        case _              => typeMismatch()
      }
    }
  }

  trait JsonVal[T] extends Extractor[T] {
    protected def isType(node: JsonNode): Boolean
    protected def cast(node: JsonNode): T

    def apply(c: Container) = c.root match {
      case n if isType(n) => cast(n)
      case _              => typeMismatch()
    }
  }

  implicit object JsonNodeVal extends JsonVal[JsonNode] {
    protected def isType(n: JsonNode) = true
    protected def cast(n: JsonNode)   = n
  }

  implicit object BoolVal extends JsonVal[Boolean] {
    protected def isType(n: JsonNode) = n.isBoolean || n.isNumber
    protected def cast(n: JsonNode)   = n.getValueAsBoolean
  }

  implicit object BinaryVal extends JsonVal[Array[Byte]] {
    protected def isType(n: JsonNode) = n.isBinary || n.isTextual
    protected def cast(n: JsonNode) = try {
      n.getBinaryValue
    } catch {
      case e: JsonParseException => typeMismatch()
    }
  }

  trait NumericJsonVal[T] extends JsonVal[T] {
    protected def isType(n: JsonNode) = n.isNumber
  }

  implicit object ByteVal extends NumericJsonVal[Byte] {
    protected def cast(n: JsonNode) = n.getIntValue.toByte
  }

  implicit object IntVal extends NumericJsonVal[Int] {
    protected def cast(n: JsonNode) = n.getIntValue
  }

  implicit object ShortVal extends NumericJsonVal[Short] {
    protected def cast(n: JsonNode) = n.getIntValue.toShort
  }

  implicit object LongVal extends NumericJsonVal[Long] {
    protected def cast(n: JsonNode) = n.getLongValue
  }

  implicit object DoubleVal extends NumericJsonVal[Double] {
    protected def cast(n: JsonNode) = n.getDoubleValue
  }

  implicit object FloatVal extends NumericJsonVal[Float] {
    protected def cast(n: JsonNode) = n.getDoubleValue.toFloat
  }

  implicit object TextVal extends JsonVal[String] {
    override def apply(c: Container) = cast(c.root)

    protected def cast(n: JsonNode) = n.getValueAsText match {
      case null => typeMismatch()
      case t    => t
    }

    // unused, since apply is overridden
    protected def isType(n: JsonNode) = true
  }

  implicit object CharVal extends JsonVal[Char] {
    override def apply(c: Container) = cast(c.root)

    protected def cast(n: JsonNode) = TextVal(n) match {
      case s if s.length == 1 => s.charAt(0)
      case _                  => typeMismatch()
    }

    // unused, since apply is overridden
    protected def isType(n: JsonNode) = true
  }
}

object Main {
  import org.codehaus.jackson.map.ObjectMapper
  import org.codehaus.jackson.JsonNode

  def time[R](f: => R) = {
    val start = System.currentTimeMillis
    val rv = f
    val end = System.currentTimeMillis

    println(end - start)
    rv
  }

  case class InReplyTo(statusId: Long, userid: Long)
  case class Retweet(parentStatusId: Long, sourceStatusId: Long)
  case class Status(id: Long, userId: Long, inReplyTo: InReplyTo, retweet: Retweet)

  implicit val retweetFromJson   = JsonObjectExtractor(Retweet, "parent_status_id", "source_status_id")
  implicit val inReplyToFromJson = JsonObjectExtractor(InReplyTo, "status_id", "user_id")
  implicit val statusFromJson    = JsonObjectExtractor(Status, "id", "user_id", "in_reply_to", "retweet")

  val mapper = new ObjectMapper

  def main(argv: Array[String]) {
    (1 to 1000) foreach { i => runIt(300000) }
  }

  def runIt(iterations: Int) {
    val json = """{
      "id": 123,
      "user_id": 456,
      "in_reply_to": {
        "status_id": 125,
        "user_id": 458
      },
      "retweet": {
        "parent_status_id": 124,
        "source_status_id": 457
      }
    }"""

    print("extractor: ")
    time {
      (1 to iterations) foreach { i =>
        statusFromJson(json)
      }
    }

    print("mapper:    ")

    time {
      (1 to iterations) foreach { i =>
        val node      = mapper.readValue(json, classOf[JsonNode])
        val rtNode    = node.get("retweet")
        val replyNode = node.get("in_reply_to")

        Status(
          node.get("id").getLongValue,
          node.get("user_id").getLongValue,
          InReplyTo(
            replyNode.get("status_id").getLongValue,
            replyNode.get("user_id").getLongValue
          ),
          Retweet(
            rtNode.get("parent_status_id").getLongValue,
            rtNode.get("source_status_id").getLongValue
          )
        )
      }
    }
  }
}
