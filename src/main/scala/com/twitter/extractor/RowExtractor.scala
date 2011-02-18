package com.twitter.extractor

import java.math.BigDecimal
import java.sql.{ResultSet, Date, Time, Timestamp, SQLException}

import exceptions._


object RowExtractor extends ExtractorFactory {
  type Container = ResultSet
  type Key       = String

  trait RowVal[T] extends ValExtractor[T] {
    def getVal(key: String, row: ResultSet): T

    def isDefinedAt(key: Key, row: Container) = try {
      getVal(key, row)
      if (row.wasNull) false else true
    } catch {
      case e: SQLException => false
    }

    def apply(key: Key, row: Container) = {
      val rv = getVal(key, row)
      if (row.wasNull) noElement(key) else rv
    }
  }

  implicit object BoolVal extends RowVal[Boolean] {
    def getVal(key: String, row: ResultSet) = row.getBoolean(key)
  }

  implicit object ByteVal extends RowVal[Byte] {
    def getVal(key: String, row: ResultSet) = row.getByte(key)
  }

  implicit object CharVal extends RowVal[Char] {
    def getVal(key: String, row: ResultSet) = row.getByte(key).toChar
  }

  implicit object ShortVal extends RowVal[Short] {
    def getVal(key: String, row: ResultSet) = row.getShort(key)
  }

  implicit object IntVal extends RowVal[Int] {
    def getVal(key: String, row: ResultSet) = row.getInt(key)
  }

  implicit object LongVal extends RowVal[Long] {
    def getVal(key: String, row: ResultSet) = row.getLong(key)
  }

  implicit object DoubleVal extends RowVal[Double] {
    def getVal(key: String, row: ResultSet) = row.getDouble(key)
  }

  implicit object FloatVal extends RowVal[Float] {
    def getVal(key: String, row: ResultSet) = row.getFloat(key)
  }

  implicit object BigDecimalVal extends RowVal[BigDecimal] {
    def getVal(key: String, row: ResultSet) = row.getBigDecimal(key)
  }

  implicit object BytesVal extends RowVal[Array[Byte]] {
    def getVal(key: String, row: ResultSet) = row.getBytes(key)
  }

  implicit object StringVal extends RowVal[String] {
    def getVal(key: String, row: ResultSet) = row.getString(key)
  }

  implicit object DateVal extends RowVal[Date] {
    def getVal(key: String, row: ResultSet) = row.getDate(key)
  }

  implicit object TimeVal extends RowVal[Time] {
    def getVal(key: String, row: ResultSet) = row.getTime(key)
  }

  implicit object TimestampVal extends RowVal[Timestamp] {
    def getVal(key: String, row: ResultSet) = row.getTimestamp(key)
  }
}
