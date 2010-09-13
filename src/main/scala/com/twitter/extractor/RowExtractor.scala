// package com.twitter.extractor

// import scala.reflect.Manifest
// import java.math.BigDecimal
// import java.sql._

// object RowExtractor extends Extractor[ResultSet, String]

// class RowVal[T](implicit protected val resultType: Manifest[T]) extends ExtractorVal[ResultSet, String, T] with OptionalType[T] {
//   def apply(row: ResultSet, col: String) = {
//     val result = wrapOptional {
//       val rowVal = getRowValue(row, col)
//       if ( row.wasNull ) noElement(col) else rowVal
//     }

//     result.asInstanceOf[T]
//   }

//   private def getRowValue[T](row: ResultSet, col: String) = {
//     innerResultType.erasure.toString match {
//       case "boolean" => row.getBoolean(col)
//       case "char" => row.getByte(col).toChar
//       case "byte" => row.getByte(col)
//       case "short" => row.getShort(col)
//       case "int" => row.getInt(col)
//       case "long" => row.getLong(col)
//       case "double" => row.getDouble(col)
//       case "float" => row.getFloat(col)
//       case "class [B" => row.getBytes(col)
//       case "class java.lang.String" => row.getString(col)
//       case "class java.math.BigDecimal" => row.getBigDecimal(col)
//       case "class java.sql.Date" => row.getDate(col)
//       case "class java.sql.Time" => row.getTime(col)
//       case "class java.sql.Timestamp" => row.getTimestamp(col)
//     }
//   }
// }

// class BoolRowVal extends RowVal[Boolean]
// class CharRowVal extends RowVal[Char]
// class ByteRowVal extends RowVal[Byte]

// object RowVal {
//   def apply[T]: RowVal[Boolean] = new BoolRowVal
// }
