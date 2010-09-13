package com.twitter.extractor

import scala.reflect.Manifest

trait MapVal[T] extends ValExtractor[(String => Any), String, T]

object MapVal {
  trait ConvertingMapVal[T] extends MapVal[T] {
    def convert(v: Any): T
    def apply(key: String) = (map: (String => Any)) => try {
      convert(map(key))
    } catch {
      case e: ClassCastException => typeMismatch(key, e)
      case e: MatchError => typeMismatch(key, e)
      case e: NoSuchMethodException => typeMismatch(key, e)
      case e: NoElementException => noElement(key)
    }
  }

  implicit object BoolVal extends ConvertingMapVal[Boolean] {
    def convert(v: Any) = v match {
      case b: Boolean => b == true
    }
  }

  implicit object ByteVal extends ConvertingMapVal[Byte] {
    def convert(v: Any) = v.asInstanceOf[{ def toByte: Byte }].toByte
  }

  implicit object ShortVal extends ConvertingMapVal[Short] {
    def convert(v: Any) = v.asInstanceOf[{ def toShort: Short }].toShort
  }

  implicit object IntVal extends ConvertingMapVal[Int] {
    def convert(v: Any) = v.asInstanceOf[{ def toInt: Int }].toInt
  }

  implicit object LongVal extends ConvertingMapVal[Long] {
    def convert(v: Any) = v.asInstanceOf[{ def toLong: Long }].toLong
  }

  implicit object DoubleVal extends ConvertingMapVal[Double] {
    def convert(v: Any) = v.asInstanceOf[{ def toDouble: Double }].toDouble
  }

  implicit object FloatVal extends ConvertingMapVal[Float] {
    def convert(v: Any) = v.asInstanceOf[{ def toFloat: Float }].toFloat
  }

  class OptionalMapVal[T](inner: MapVal[T]) extends MapVal[Option[T]] {
    def apply(key: String) = (map: (String => Any)) => try {
      Some(inner(key)(map)).asInstanceOf[Option[T]]
    } catch {
      case e: NoElementException => None
    }
  }

  implicit def optionalMapVal[T, OptionT <: Option[T]](implicit inner: MapVal[T]) = new OptionalMapVal[T](inner)

  class AnyRefVal[T <: AnyRef] extends ConvertingMapVal[T] {
    def convert(v: Any) = v.asInstanceOf[T]
  }

  implicit def anyRefVal[T <: AnyRef] = new AnyRefVal[T]
}

object MapExtractor extends Extractor {
  type Container = String => Any
  type Key = String
  type VE[T] = MapVal[T]
}
