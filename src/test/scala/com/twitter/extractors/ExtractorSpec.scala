package com.twitter.extractors

import org.specs.Specification
import org.specs.mock.{ClassMocker, JMocker}

import com.twitter.extractors.map.MapExtractor
import com.twitter.extractors.sql.RowExtractor
import com.twitter.extractors.json.JsonObjectExtractor


case class OneBool(v: Boolean)
object OneBool extends (Boolean => OneBool) {
  implicit val fromMap = MapExtractor(apply, "bool")
  implicit val fromRow = RowExtractor(OneBool, "c1")
}

case class OneChar(v: Char)
object OneChar extends (Char => OneChar) {
  implicit val fromMap = MapExtractor(apply, "char")
}

case class OneByte(v: Byte)
object OneByte extends (Byte => OneByte) {
  implicit val fromMap = MapExtractor(apply, "byte")
}

case class OneShort(v: Short)
object OneShort extends (Short => OneShort) {
  implicit val fromMap = MapExtractor(apply, "short")
}

case class OneInt(v: Int)
object OneInt extends (Int => OneInt) {
  implicit val fromMap = MapExtractor(apply, "int")
}

case class OneLong(v: Long)
object OneLong extends (Long => OneLong) {
  implicit val fromMap = MapExtractor(apply, "long")
  implicit val fromJson = JsonObjectExtractor(OneLong, "long")
}

case class OneDouble(v: Double)
object OneDouble extends (Double => OneDouble) {
  implicit val fromMap = MapExtractor(apply, "double")
}

case class OneFloat(v: Float)
object OneFloat extends (Float => OneFloat) {
  implicit val fromMap = MapExtractor(apply, "float")
}

case class OneString(v: String)
object OneString extends (String => OneString) {
  implicit val fromMap = MapExtractor(apply, "string")
}

case class OneOpt(v: Option[Float])
object OneOpt extends (Option[Float] => OneOpt) {
  implicit val fromMap = MapExtractor(apply, "float")
}

case class MapAndLong(v1: OneFloat, v2: Long)
object MapAndLong extends ((OneFloat, Long) => MapAndLong) {
  implicit val fromMap = MapExtractor(apply, "one_float", "long")
}


object MapExtractorSpec extends Specification {
  "extracts" in {
    "object" in {
      OneString.fromMap(Map("string" -> "hi there")) mustEqual OneString("hi there")
    }

    "boolean" in {
      OneBool.fromMap(Map("bool" -> true)) mustEqual OneBool(true)
      OneBool.fromMap(Map("bool" -> true)) mustEqual OneBool(true)
      OneBool.fromMap(Map("bool" -> "foo")) must throwA[TypeMismatchException]
      OneBool.fromMap(Map("foo" -> true)) must throwA(new NoSuchElementException("key not found: bool"))
    }

    "char" in {
      OneChar.fromMap(Map("char" -> 30)) mustEqual OneChar(30)
      OneChar.fromMap(Map("char" -> 0)) mustEqual OneChar(0)
      OneChar.fromMap(Map("char" -> "foo")) must throwA[TypeMismatchException]
    }

    "byte" in {
      OneByte.fromMap(Map("byte" -> 30)) mustEqual OneByte(30)
      OneByte.fromMap(Map("byte" -> 0)) mustEqual OneByte(0)
      OneByte.fromMap(Map("byte" -> "foo")) must throwA[TypeMismatchException]
    }

    "short" in {
      OneShort.fromMap(Map("short" -> 30)) mustEqual OneShort(30)
      OneShort.fromMap(Map("short" -> 0)) mustEqual OneShort(0)
      OneShort.fromMap(Map("short" -> "foo")) must throwA[TypeMismatchException]
    }

    "int" in {
      OneInt.fromMap(Map("int" -> 30)) mustEqual OneInt(30)
      OneInt.fromMap(Map("int" -> 0)) mustEqual OneInt(0)
      OneInt.fromMap(Map("int" -> "foo")) must throwA[TypeMismatchException]
    }

    "long" in {
      OneLong.fromMap(Map("long" -> 30)) mustEqual OneLong(30)
      OneLong.fromMap(Map("long" -> 0)) mustEqual OneLong(0)
      OneLong.fromMap(Map("long" -> "foo")) must throwA[TypeMismatchException]
    }

    "double" in {
      OneDouble.fromMap(Map("double" -> 30)) mustEqual OneDouble(30)
      OneDouble.fromMap(Map("double" -> 0)) mustEqual OneDouble(0)
      OneDouble.fromMap(Map("double" -> "foo")) must throwA[TypeMismatchException]
    }

    "float" in {
      OneFloat.fromMap(Map("float" -> 30)) mustEqual OneFloat(30)
      OneFloat.fromMap(Map("float" -> 0)) mustEqual OneFloat(0)
      OneFloat.fromMap(Map("float" -> "foo")) must throwA[TypeMismatchException]
    }

    "option" in {
      OneOpt.fromMap(Map("float" -> 30)) mustEqual OneOpt(Some(30))
      OneOpt.fromMap(Map("notfloat" -> 30)) mustEqual OneOpt(None)
      OneOpt.fromMap(Map("float" -> "foo")) must throwA[TypeMismatchException]
    }

    "inner extractor" in {
      val map = Map("one_float" -> Map("float" -> 1.0), "long" -> 2)
      MapAndLong.fromMap(map) mustEqual MapAndLong(OneFloat(1), 2)
    }

    "custom extractors" in {
      implicit val oneFloatExtractor = MapExtractor(OneFloat, "alt_float")
      val extractor = MapExtractor(MapAndLong, "of", "a_long")

      val map = Map("of" -> Map("alt_float" -> 1.0), "a_long" -> 2)
      extractor(map) mustEqual MapAndLong(OneFloat(1), 2)
    }
  }
}


case class OneByteArray(v: Array[Byte])
object OneByteArray extends (Array[Byte] => OneByteArray) {
  implicit val fromMap = MapExtractor(apply, "byte_array")
  implicit val fromRow = RowExtractor(OneByteArray, "c1")
}

object RowExtractorSpec extends Specification with JMocker with ClassMocker {
  import java.sql.ResultSet

  val resultSet = mock[ResultSet]

  "unnested bool works" in {
    expect {
      one(resultSet).getBoolean("c1") willReturn true
      one(resultSet).wasNull          willReturn false
    }

    OneBool.fromRow(resultSet) mustEqual OneBool(true)
  }

  "bytearray works" in {
    val bytes = Array(1,2,3).map(_.toByte)

    expect {
      one(resultSet).getBytes("c1") willReturn bytes
      one(resultSet).wasNull        willReturn false
    }

    OneByteArray.fromRow(resultSet) mustEqual OneByteArray(bytes)
  }
}


case class Structured(s: String, f: Double, i: Int, ol: OneLong)
object Structured extends ((String, Double, Int, OneLong) => Structured) {
  implicit val fromJson = JsonObjectExtractor(Structured, "string", "double", "int", "one_long")
  val fromUncheckedJson = JsonObjectExtractor(Structured, "string", "double", "int", "one_long")
}

object JsonExtractorSpec extends Specification {
  "works" in {
    val json = """{ "string" : "foo", "int" : 1, "double" : 2.3, "one_long" : { "long" : 23 } }"""

    Structured.fromJson(json) mustEqual Structured("foo", 2.3, 1, OneLong(23L))
  }
}
