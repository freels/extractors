package com.twitter.extractors
package map

import scala.collection.generic.CanBuild
import com.twitter.util.{Try, Return, Throw}
import com.twitter.extractors.exceptions._


protected trait ObjectExtractorLow extends {
  protected class AnyExtractor[T] extends ObjectExtractor.ObjectExtractor[T] {
    def convert(v: Any) = v.asInstanceOf[T]
  }

  implicit def anyVal[T]: ObjectExtractor.Extractor[T] = new AnyExtractor[T]
}

object ObjectExtractor extends ExtractorFactory
with ObjectExtractorLow
with LiftedExtractors
with KeyExtractors
with IterableExtractors {

  type Container = Any
  type Key       = String

  def keyMapper(c: Container, key: Key) = c.asInstanceOf[Map[String,Any]].get(key)

  def iterableMapper(c: Container) = c.asInstanceOf[Iterable[Container]]

  trait ObjectExtractor[R] extends Extractor[R] {
    def convert(c: Container): R

    def apply(c: Container) = try {
      convert(c)
    } catch {
      case e: MatchError            => typeMismatch()
      case e: NoSuchMethodException => typeMismatch()
      case e: ClassCastException    => typeMismatch()
    }
  }

  implicit object BoolVal extends ObjectExtractor[Boolean] {
    def convert(v: Any) = v match { case b: Boolean => b == true }
  }

  implicit object ByteVal extends ObjectExtractor[Byte] {
    def convert(v: Any) = v.asInstanceOf[{ def toByte: Byte }].toByte
  }

  implicit object CharVal extends ObjectExtractor[Char] {
    def convert(v: Any) = v.asInstanceOf[{ def toChar: Char }].toChar
  }

  implicit object ShortVal extends ObjectExtractor[Short] {
    def convert(v: Any) = v.asInstanceOf[{ def toShort: Short }].toShort
  }

  implicit object IntVal extends ObjectExtractor[Int] {
    def convert(v: Any) = v.asInstanceOf[{ def toInt: Int }].toInt
  }

  implicit object LongVal extends ObjectExtractor[Long] {
    def convert(v: Any) = v.asInstanceOf[{ def toLong: Long }].toLong
  }

  implicit object DoubleVal extends ObjectExtractor[Double] {
    def convert(v: Any) = v.asInstanceOf[{ def toDouble: Double }].toDouble
  }

  implicit object FloatVal extends ObjectExtractor[Float] {
    def convert(v: Any) = v.asInstanceOf[{ def toFloat: Float }].toFloat
  }
}

