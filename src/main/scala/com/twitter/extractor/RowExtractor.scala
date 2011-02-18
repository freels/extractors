package com.twitter.extractor

import java.math.BigDecimal
import java.sql.{ResultSet, Date, Time, Timestamp, SQLException}

trait RowVal[+T] extends ValExtractor[ResultSet, String, T] {
  def isDefinedAt(kr: ApplyType) = kr match { case (k, r) =>
    try {
      r.findColumn(k)
      true
    } catch {
      case e: SQLException => false
    }
  }
}

object RowVal {
  trait ConvertingRowVal[+T] extends RowVal[T] {
    def getVal(row: ResultSet, key: String): T
    def apply(kr: ApplyType) = {
      val (key, row) = kr
      val rv = getVal(row, key)
      if ( row.wasNull ) noElement(key) else rv
    }
  }

  class OptionalRowVal[+T](inner: RowVal[T]) extends RowVal[Option[T]] {
    def apply(kr: ApplyType) = {
      val (key, row) = kr
      try {
        Some(inner(key, row))
      } catch {
        case e: NoElementException => None
      }
    }
  }

  implicit def optionalRowMap[T, OptT <: Option[T]](implicit inner: RowVal[T]) = {
    new OptionalRowVal[T](inner)
  }

  implicit object BoolVal extends ConvertingRowVal[Boolean] {
    def getVal(row: ResultSet, key: String) = row.getBoolean(key)
  }

  implicit object ByteVal extends ConvertingRowVal[Byte] {
    def getVal(row: ResultSet, key: String) = row.getByte(key)
  }

  implicit object CharVal extends ConvertingRowVal[Char] {
    def getVal(row: ResultSet, key: String) = row.getByte(key).toChar
  }

  implicit object ShortVal extends ConvertingRowVal[Short] {
    def getVal(row: ResultSet, key: String) = row.getShort(key)
  }

  implicit object IntVal extends ConvertingRowVal[Int] {
    def getVal(row: ResultSet, key: String) = row.getInt(key)
  }

  implicit object LongVal extends ConvertingRowVal[Long] {
    def getVal(row: ResultSet, key: String) = row.getLong(key)
  }

  implicit object DoubleVal extends ConvertingRowVal[Double] {
    def getVal(row: ResultSet, key: String) = row.getDouble(key)
  }

  implicit object FloatVal extends ConvertingRowVal[Float] {
    def getVal(row: ResultSet, key: String) = row.getFloat(key)
  }

  implicit object BigDecimalVal extends ConvertingRowVal[BigDecimal] {
    def getVal(row: ResultSet, key: String) = row.getBigDecimal(key)
  }

  implicit object BytesVal extends ConvertingRowVal[Array[Byte]] {
    def getVal(row: ResultSet, key: String) = row.getBytes(key)
  }

  implicit object StringVal extends ConvertingRowVal[String] {
    def getVal(row: ResultSet, key: String) = row.getString(key)
  }

  implicit object DateVal extends ConvertingRowVal[Date] {
    def getVal(row: ResultSet, key: String) = row.getDate(key)
  }

  implicit object TimeVal extends ConvertingRowVal[Time] {
    def getVal(row: ResultSet, key: String) = row.getTime(key)
  }

  implicit object TimestampVal extends ConvertingRowVal[Timestamp] {
    def getVal(row: ResultSet, key: String) = row.getTimestamp(key)
  }
}

object RowExtractor extends ExtractorFactory {
  type Container = ResultSet
  type Key = String
  type VE[+T] = RowVal[T]
}
