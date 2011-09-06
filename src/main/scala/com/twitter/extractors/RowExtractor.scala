// package com.twitter.extractors
// package sql

// import java.math.BigDecimal
// import java.sql.{ResultSet, Date, Time, Timestamp, SQLException}

// import exceptions._

// class RowContext(rs: ResultSet, path: Seq[String]) {
//   def this(rs: ResultSet) = this(rs, Seq())

//   private lazy val colName = {
//     assert(!path.isEmpty)
//     path.mkString("_")
//   }

//   def /(segment: String) = new RowContext(rs, path :+ segment)

//   def unlessNull[T](t: => T): T = {
//     val rv = t
//     if (rs.wasNull) noElement(colName) else rv
//   }

//   def getBoolean    = unlessNull { rs.getBoolean(colName) }
//   def getByte       = unlessNull { rs.getByte(colName) }
//   def getShort      = unlessNull { rs.getShort(colName) }
//   def getInt        = unlessNull { rs.getShort(colName) }
//   def getLong       = unlessNull { rs.getLong(colName) }
//   def getDouble     = unlessNull { rs.getDouble(colName) }
//   def getFloat      = unlessNull { rs.getFloat(colName) }
//   def getBigDecimal = unlessNull { rs.getBigDecimal(colName) }
//   def getBytes      = unlessNull { rs.getBytes(colName) }
//   def getString     = unlessNull { rs.getString(colName) }
//   def getDate       = unlessNull { rs.getDate(colName) }
//   def getTime       = unlessNull { rs.getTime(colName) }
//   def getTimestamp  = unlessNull { rs.getTimestamp(colName) }
// }



// object RowExtractor extends ExtractorFactory with NestedExtractors {
//   type Root      = ResultSet
//   type Container = RowContext
//   type Key       = String

//   def liftRoot(r: Root) = new RowContext(r)
//   def containerForKey(c: Container, k: Key) = c / k

//   trait RowVal[T] extends Extractor[T]

//   implicit object BoolVal extends RowVal[Boolean] {
//     def apply(c: Container) = c.getBoolean
//   }

//   implicit object ByteVal extends RowVal[Byte] {
//     def apply(c: Container) = c.getByte
//   }

//   implicit object CharVal extends RowVal[Char] {
//     def apply(c: Container) = c.getByte.toChar
//   }

//   implicit object ShortVal extends RowVal[Short] {
//     def apply(c: Container) = c.getShort
//   }

//   implicit object IntVal extends RowVal[Int] {
//     def apply(c: Container) = c.getInt
//   }

//   implicit object LongVal extends RowVal[Long] {
//     def apply(c: Container) = c.getLong
//   }

//   implicit object DoubleVal extends RowVal[Double] {
//     def apply(c: Container) = c.getDouble
//   }

//   implicit object FloatVal extends RowVal[Float] {
//     def apply(c: Container) = c.getFloat
//   }

//   implicit object BigDecimalVal extends RowVal[BigDecimal] {
//     def apply(c: Container) = c.getBigDecimal
//   }

//   implicit object BytesVal extends RowVal[Array[Byte]] {
//     def apply(c: Container) = c.getBytes
//   }

//   implicit object StringVal extends RowVal[String] {
//     def apply(c: Container) = c.getString
//   }

//   implicit object DateVal extends RowVal[Date] {
//     def apply(c: Container) = c.getDate
//   }

//   implicit object TimeVal extends RowVal[Time] {
//     def apply(c: Container) = c.getTime
//   }

//   implicit object TimestampVal extends RowVal[Timestamp] {
//     def apply(c: Container) = c.getTimestamp
//   }
// }

