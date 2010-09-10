package com.twitter.extractor

import scala.reflect.Manifest

object MapExtractor extends Extractor[(String => Any), String]

case class MVal[T](implicit m: Manifest[T]) extends Extractor.Val[(String => Any), String, T] {
  val valMatch = """^(boolean|char|byte|short|int|long|double|float)$""".r

  def apply(map: (String => Any), key: String) = {
    println(m.erasure)
    m.erasure.toString match {
      case valMatch(v) => getVal(map(key))(m)
      case _ => getRef(map(key))(m)
    }
  }

  private def getRef[T](v: Any)(implicit m: Manifest[T]): T = v match {
    case ref: AnyRef => ref.asInstanceOf[T]
    case _ => error("val is not an AnyRef")
  }

  private def getVal[T](v: Any)(implicit m: Manifest[T]): T = {
    try {
      val converted = m.erasure.toString match {
        case "boolean" => v == true
        case "char" => v.asInstanceOf[{ def toChar: Char }].toChar
        case "byte" => v.asInstanceOf[{ def toByte: Byte }].toByte
        case "short" => v.asInstanceOf[{ def toShort: Short }].toShort
        case "int" => v.asInstanceOf[{def toInt: Int }].toInt
        case "long" => v.asInstanceOf[{def toLong: Long }].toLong
        case "double" => v.asInstanceOf[{def toDouble: Double }].toDouble
        case "float" => v.asInstanceOf[{def toFloat: Float }].toFloat
      }

      converted.asInstanceOf[T]
    } catch {
      case e: java.lang.NoSuchMethodException => error("ref is not an AnyVal")
    }
  }
}
