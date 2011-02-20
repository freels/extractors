package com.twitter.extractors
package json

import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.JsonNode

import exceptions._


class JsonRoot(val root: JsonNode)

object JsonRoot {
  val mapper = new ObjectMapper

  protected def parse(s: String) = mapper.readValue(s, classOf[JsonNode])

  implicit def string2Json(s: String) = new JsonRoot(parse(s))
  implicit def bytes2Json(b: Array[Byte]) = new JsonRoot(parse(new String(b, "UTF-8")))
  implicit def parsed2Json(p: JsonNode) = new JsonRoot(p)
}

object JsonExtractor extends ExtractorFactory with NestedExtractors {
  type Container = JsonRoot
  type Key       = String

  def getFromContainer[R](e: Extractor[R], k: Key, j: Container) = {
    j.root.get(k) match {
      case null            => noElement(k)
      case n if n.isObject => e(new JsonRoot(n))
      case _               => typeMismatch(k, null)
    }
  }

  trait JsonVal[T] extends ValExtractor[T] {
    protected def isType(node: JsonNode): Boolean
    protected def cast(node: JsonNode): T

    def apply(key: Key, j: Container) = {
      j.root.get(key) match {
        case null           => noElement(key)
        case n if isType(n) => cast(n)
        case _              => typeMismatch(key, null)
      }
    }
  }

  implicit object BoolVal extends JsonVal[Boolean] {
    protected def isType(n: JsonNode) = n.isBoolean || n.isNumber
    protected def cast(n: JsonNode)   = n.getValueAsBoolean
  }

  trait NumericJsonVal[T] extends JsonVal[T] {
    protected def isType(n: JsonNode) = n.isNumber
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

  implicit object StringVal extends JsonVal[String] {
    override def apply(key: Key, j: Container) = {
      j.root.get(key) match {
        case null => noElement(key)
        case n => n.getValueAsText match {
          case null => typeMismatch(key, null)
          case t    => t
        }
      }
    }

    // unused, since apply is overridden
    protected def isType(n: JsonNode) = true
    protected def cast(n: JsonNode)   = n.getValueAsText
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

  implicit val retweetFromJson   = JsonExtractor(Retweet, "parent_status_id", "source_status_id")
  implicit val inReplyToFromJson = JsonExtractor(InReplyTo, "status_id", "user_id")
  implicit val statusFromJson    = JsonExtractor(Status, "id", "user_id", "in_reply_to", "retweet")

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
