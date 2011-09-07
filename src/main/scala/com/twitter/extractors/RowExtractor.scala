package com.twitter.extractors
package sql

import java.math.BigDecimal
import java.sql.{ResultSet, Date, Time, Timestamp, SQLException}

import exceptions._

object RowContext {
  implicit def resultSetToRowContext(rs: ResultSet): RowContext = {
    new RowContext(rs, Seq())
  }
}

class RowContext(rs: ResultSet, path: Seq[String]) {

  private lazy val colName = {
    assert(!path.isEmpty)
    path.mkString("_")
  }

  def /(segment: String) = new RowContext(rs, path :+ segment)

  def unlessNull[T](t: => T): T = {
    val rv = t
    if (rs.wasNull) noElement(colName) else rv
  }

  def getBoolean    = unlessNull { rs.getBoolean(colName) }
  def getByte       = unlessNull { rs.getByte(colName) }
  def getShort      = unlessNull { rs.getShort(colName) }
  def getInt        = unlessNull { rs.getShort(colName) }
  def getLong       = unlessNull { rs.getLong(colName) }
  def getDouble     = unlessNull { rs.getDouble(colName) }
  def getFloat      = unlessNull { rs.getFloat(colName) }
  def getBigDecimal = unlessNull { rs.getBigDecimal(colName) }
  def getBytes      = unlessNull { rs.getBytes(colName) }
  def getString     = unlessNull { rs.getString(colName) }
  def getDate       = unlessNull { rs.getDate(colName) }
  def getTime       = unlessNull { rs.getTime(colName) }
  def getTimestamp  = unlessNull { rs.getTimestamp(colName) }
}



object RowExtractor extends ExtractorFactory with KeyedExtractors {
  type Container = RowContext
  type Key       = String

  def KeyExtractor[R](key: Key, inner: Extractor[R]) = new SubcontainerExtractor[R](inner) {
    def subcontainer(c: Container) = c / k
  }

  implicit object BoolVal extends Extractor[Boolean] {
    def apply(c: Container) = c.getBoolean
  }

  implicit object ByteVal extends Extractor[Byte] {
    def apply(c: Container) = c.getByte
  }

  implicit object CharVal extends Extractor[Char] {
    def apply(c: Container) = c.getByte.toChar
  }

  implicit object ShortVal extends Extractor[Short] {
    def apply(c: Container) = c.getShort
  }

  implicit object IntVal extends Extractor[Int] {
    def apply(c: Container) = c.getInt
  }

  implicit object LongVal extends Extractor[Long] {
    def apply(c: Container) = c.getLong
  }

  implicit object DoubleVal extends Extractor[Double] {
    def apply(c: Container) = c.getDouble
  }

  implicit object FloatVal extends Extractor[Float] {
    def apply(c: Container) = c.getFloat
  }

  implicit object BigDecimalVal extends Extractor[BigDecimal] {
    def apply(c: Container) = c.getBigDecimal
  }

  implicit object BytesVal extends Extractor[Array[Byte]] {
    def apply(c: Container) = c.getBytes
  }

  implicit object StringVal extends Extractor[String] {
    def apply(c: Container) = c.getString
  }

  implicit object DateVal extends Extractor[Date] {
    def apply(c: Container) = c.getDate
  }

  implicit object TimeVal extends Extractor[Time] {
    def apply(c: Container) = c.getTime
  }

  implicit object TimestampVal extends Extractor[Timestamp] {
    def apply(c: Container) = c.getTimestamp
  }
}

