package com.twitter.extractor

import scala.reflect.Manifest

object MapExtractor extends Extractor[(String => Any), String]

case class MVal[T](implicit protected val resultType: Manifest[T]) extends ExtractorVal[(String => Any), String, T] with OptionalType[T] {

  def apply(map: (String => Any), key: String) = {
    val result = wrapOptional {
      try { getAny(map(key)) }
      catch {
        case e: NoSuchElementException => noElement(key)
      }
    }

    result.asInstanceOf[T]
  }

  private def getAny(v: Any) = {
    innerResultType.erasure.toString match {
      case valMatch(_) => getVal(v)
      case _ => getRef(v)
    }
  }

  private def getRef(v: Any): AnyRef = v match {
    case ref: AnyRef => ref
    case _ => error("val is not an AnyRef")
  }

  private def getVal[T](v: Any): AnyVal = {
    try {
      innerResultType.erasure.toString match {
        case "boolean" => {
          v match {
            case b: Boolean => b == true
            case _ => v.asInstanceOf[{ def > (o: Int): Boolean }] > 0
          }
        }
        case "char" => v.asInstanceOf[{ def toChar: Char }].toChar
        case "byte" => v.asInstanceOf[{ def toByte: Byte }].toByte
        case "short" => v.asInstanceOf[{ def toShort: Short }].toShort
        case "int" => v.asInstanceOf[{def toInt: Int }].toInt
        case "long" => v.asInstanceOf[{def toLong: Long }].toLong
        case "double" => v.asInstanceOf[{def toDouble: Double }].toDouble
        case "float" => v.asInstanceOf[{def toFloat: Float }].toFloat
      }
    } catch {
      case e: NoSuchMethodException => error("ref is not an AnyVal")
    }
  }
}
