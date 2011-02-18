package com.twitter.extractors

import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.JsonNode

import exceptions._


package object json {

  def extract [R: JsonExtractor.Extractor](v: JsonExtractor.Container): R = {
    val extractor = implicitly[JsonExtractor.Extractor[R]]
    extractor(v)
  }


  sealed abstract class JsonRoot {
    def root: JsonNode
  }

  class StringJsonRoot(unparsed: String) extends JsonRoot {
    lazy val root = JsonRoot.mapper.readValue(unparsed, classOf[JsonNode])
  }

  class ParsedJsonRoot(val root: JsonNode) extends JsonRoot

  object JsonRoot {
    val mapper = new ObjectMapper

    implicit def string2Json(s: String) = new StringJsonRoot(s)
    implicit def bytes2Json(b: Array[Byte]) = new StringJsonRoot(new String(b, "UTF-8"))
    implicit def parsed2Json(p: JsonNode) = new ParsedJsonRoot(p)
  }

  object JsonExtractor extends ExtractorFactory with NestedExtractors {
    type Container = JsonRoot
    type Key       = String

    def getFromContainer(k: Key, j: Container): Container = {
      val node = j.root.path(k)
      if (node.isObject) new ParsedJsonRoot(node) else noElement(k)
    }

    def containerIsDefinedAt(k: Key, j: Container) = j.root.path(k).isObject

    trait JsonVal[T] extends ValExtractor[T] {
      def cast(node: JsonNode): Option[T]

      def isDefinedAt(key: Key, j: Container) = j.root.path(key).isMissingNode

      def apply(key: Key, j: Container) = {
        val rv = j.root.path(key)
        if (rv.isMissingNode) noElement(key)
        else cast(rv) getOrElse typeMismatch(key, null)
      }
    }

    implicit object BoolVal extends JsonVal[Boolean] {
      def cast(node: JsonNode) = if (node.isBoolean) Some(node.getValueAsBoolean) else None
    }

    implicit object IntVal extends JsonVal[Int] {
      def cast(node: JsonNode) = if (node.isInt) Some(node.getValueAsInt) else None
    }

    implicit object LongVal extends JsonVal[Long] {
      def cast(node: JsonNode) = if (node.isInt || node.isLong) Some(node.getValueAsLong) else None
    }

    implicit object DoubleVal extends JsonVal[Double] {
      def cast(node: JsonNode) = if (node.isDouble) Some(node.getValueAsDouble) else None
    }

    implicit object FloatVal extends JsonVal[Float] {
      def cast(node: JsonNode) = if (node.isDouble) Some(node.getValueAsDouble.toFloat) else None
    }

    implicit object StringVal extends JsonVal[String] {
      def cast(node: JsonNode) = if (node.isTextual) Some(node.getValueAsText) else None
    }
  }
}
