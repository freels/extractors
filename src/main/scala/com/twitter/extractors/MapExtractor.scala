package com.twitter.extractors
package map

import com.twitter.util.{Try, Return, Throw}
import com.twitter.extractors.exceptions._


protected trait ObjectExtractorLow {
  protected class AnyExtractor[T] extends ObjectExtractor.Extractor[T] {
    def apply(v: Any) = v.asInstanceOf[T]
  }

  implicit def anyVal[T]: ObjectExtractor.Extractor[T] = new AnyExtractor[T]
}

object ObjectExtractor extends ExtractorFactory with ObjectExtractorLow with KeyedExtractors {
  type Container = Any
  type Key       = String

  def KeyExtractor[R](key: Key, inner: Extractor[R]) = new SubcontainerExtractor[R](inner) {
    def subcontainer(c: Container) = c.asInstanceOf[Map[String,Any]].get(key)
  }

  implicit object BoolVal extends ObjectExtractor.Extractor[Boolean] {
    def apply(v: Any) = v match { case b: Boolean => b == true }
  }

  implicit object ByteVal extends ObjectExtractor.Extractor[Byte] {
    def apply(v: Any) = v.asInstanceOf[{ def toByte: Byte }].toByte
  }

  implicit object CharVal extends ObjectExtractor.Extractor[Char] {
    def apply(v: Any) = v.asInstanceOf[{ def toChar: Char }].toChar
  }

  implicit object ShortVal extends ObjectExtractor.Extractor[Short] {
    def apply(v: Any) = v.asInstanceOf[{ def toShort: Short }].toShort
  }

  implicit object IntVal extends ObjectExtractor.Extractor[Int] {
    def apply(v: Any) = v.asInstanceOf[{ def toInt: Int }].toInt
  }

  implicit object LongVal extends ObjectExtractor.Extractor[Long] {
    def apply(v: Any) = v.asInstanceOf[{ def toLong: Long }].toLong
  }

  implicit object DoubleVal extends ObjectExtractor.Extractor[Double] {
    def apply(v: Any) = v.asInstanceOf[{ def toDouble: Double }].toDouble
  }

  implicit object FloatVal extends ObjectExtractor.Extractor[Float] {
    def apply(v: Any) = v.asInstanceOf[{ def toFloat: Float }].toFloat
  }
}

