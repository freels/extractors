package com.twitter.extractor

import java.math.BigDecimal
import java.sql.{ResultSet, Date, Time, Timestamp, SQLException}


object RowExtractor extends ExtractorFactory {
  type Container = ResultSet
  type Key = String

  trait RowVal[T] extends ValExtractor[T] {
    def getVal(row: ResultSet, key: String): T

    def isDefinedAt(kr: (Key, Container)) = kr match { case (k,r) =>
      try {
        r.findColumn(k)
        true
      } catch {
        case e: SQLException => false
      }
    }

    def apply(kr: (Key, Container)) = {
      val (key, row) = kr
      val rv = getVal(row, key)
      if ( row.wasNull ) noElement(key) else rv
    }
  }

  implicit object BoolVal extends RowVal[Boolean] {
    def getVal(row: ResultSet, key: String) = row.getBoolean(key)
  }

  implicit object ByteVal extends RowVal[Byte] {
    def getVal(row: ResultSet, key: String) = row.getByte(key)
  }

  implicit object CharVal extends RowVal[Char] {
    def getVal(row: ResultSet, key: String) = row.getByte(key).toChar
  }

  implicit object ShortVal extends RowVal[Short] {
    def getVal(row: ResultSet, key: String) = row.getShort(key)
  }

  implicit object IntVal extends RowVal[Int] {
    def getVal(row: ResultSet, key: String) = row.getInt(key)
  }

  implicit object LongVal extends RowVal[Long] {
    def getVal(row: ResultSet, key: String) = row.getLong(key)
  }

  implicit object DoubleVal extends RowVal[Double] {
    def getVal(row: ResultSet, key: String) = row.getDouble(key)
  }

  implicit object FloatVal extends RowVal[Float] {
    def getVal(row: ResultSet, key: String) = row.getFloat(key)
  }

  implicit object BigDecimalVal extends RowVal[BigDecimal] {
    def getVal(row: ResultSet, key: String) = row.getBigDecimal(key)
  }

  implicit object BytesVal extends RowVal[Array[Byte]] {
    def getVal(row: ResultSet, key: String) = row.getBytes(key)
  }

  implicit object StringVal extends RowVal[String] {
    def getVal(row: ResultSet, key: String) = row.getString(key)
  }

  implicit object DateVal extends RowVal[Date] {
    def getVal(row: ResultSet, key: String) = row.getDate(key)
  }

  implicit object TimeVal extends RowVal[Time] {
    def getVal(row: ResultSet, key: String) = row.getTime(key)
  }

  implicit object TimestampVal extends RowVal[Timestamp] {
    def getVal(row: ResultSet, key: String) = row.getTimestamp(key)
  }
}
