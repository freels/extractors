package com.twitter.extractors
package map

import exceptions._

protected trait MapExtractorLow {
  protected class AnyExtractor[T] extends MapExtractor.MapVal[T] {
    def convert(v: Any) = v.asInstanceOf[T]
  }

  implicit def anyVal[T]: MapExtractor.ValExtractor[T] = new AnyExtractor[T]
}

object MapExtractor extends ExtractorFactory with NestedExtractors with MapExtractorLow {
  type Root      = PartialFunction[String, Any]
  type Container = Any
  type Key       = String


  def liftRoot(r: Root) = r.asInstanceOf[Any]
  def getWithKey(k: Key, c: Container) = c.asInstanceOf[Root](k)

  trait MapVal[T] extends MapExtractor.ValExtractor[T] {
    def convert(v: Any): T

    def apply(c: Container): T = try { convert(c) } catch {
      case e: ClassCastException => typeMismatch()
      case e: MatchError => typeMismatch()
      case e: NoSuchMethodException => typeMismatch()
    }
  }

  implicit object BoolVal extends MapExtractor.MapVal[Boolean] {
    def convert(v: Any) = v match { case b: Boolean => b == true }
  }

  implicit object ByteVal extends MapExtractor.MapVal[Byte] {
    def convert(v: Any) = v.asInstanceOf[{ def toByte: Byte }].toByte
  }

  implicit object CharVal extends MapExtractor.MapVal[Char] {
    def convert(v: Any) = v.asInstanceOf[{ def toChar: Char }].toChar
  }

  implicit object ShortVal extends MapExtractor.MapVal[Short] {
    def convert(v: Any) = v.asInstanceOf[{ def toShort: Short }].toShort
  }

  implicit object IntVal extends MapExtractor.MapVal[Int] {
    def convert(v: Any) = v.asInstanceOf[{ def toInt: Int }].toInt
  }

  implicit object LongVal extends MapExtractor.MapVal[Long] {
    def convert(v: Any) = v.asInstanceOf[{ def toLong: Long }].toLong
  }

  implicit object DoubleVal extends MapExtractor.MapVal[Double] {
    def convert(v: Any) = v.asInstanceOf[{ def toDouble: Double }].toDouble
  }

  implicit object FloatVal extends MapExtractor.MapVal[Float] {
    def convert(v: Any) = v.asInstanceOf[{ def toFloat: Float }].toFloat
  }
}

