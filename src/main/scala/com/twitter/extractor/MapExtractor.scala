package com.twitter.extractor


object MapExtractor extends ExtractorFactory with NestedExtractors with ValConversions {
  type Container = PartialFunction[String, Any]
  type Key       = String
  //type V[T]  MapVal[T]

  def getFromContainer[R](kc: (Key, Container)) = kc match { case (k,c) => c(k).asInstanceOf[R] }
  def containerIsDefinedAt(kc: (Key, Container)) = kc match { case (k,c) => c.isDefinedAt(k) }

  trait MapVal[T] extends MapExtractor.ValExtractor[T] {
    def convert(v: Any): T

    def isDefinedAt(km: (Key, Container)) = km match { case (k, m) => m.isDefinedAt(k) }

    def apply(km: (Key, Container)): T = {
      val (key, map) = km
      try {
        convert(map(key))
      } catch {
        case e: ClassCastException => typeMismatch(key, e)
        case e: MatchError => typeMismatch(key, e)
        case e: NoSuchMethodException => typeMismatch(key, e)
      }
    }
  }
}

protected trait DefaultAnyConversion {
  protected class AnyExtractor[T] extends MapExtractor.MapVal[T] {
    def convert(v: Any) = v.asInstanceOf[T]
  }

  implicit def anyVal[T]: MapExtractor.ValExtractor[T] = new AnyExtractor[T]
}

protected trait ValConversions extends DefaultAnyConversion {
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
