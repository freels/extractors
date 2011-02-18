package com.twitter.extractors

import org.specs.Specification
import org.specs.mock.{ClassMocker, JMocker}

import com.twitter.extractors.map.MapExtractor
import com.twitter.extractors.sql.RowExtractor
import com.twitter.extractors.json.JsonExtractor

case class OneBool(v: Boolean)

object OneBool extends (Boolean => OneBool) {
  implicit val mapExtractor = MapExtractor(apply, "a_bool")
}

case class OneChar(v: Char)
case class OneByte(v: Byte)
case class OneShort(v: Short)
case class OneInt(v: Int)
case class OneLong(v: Long)
case class OneDouble(v: Double)
case class OneFloat(v: Float)
case class OneString(v: String)
case class OneOpt(v: Option[Float])
case class MapAndLong(v1: OneFloat, v2: Long)


object MapExtractorSpec extends Specification {
  "extracts" in {
    "object" in {
      val extractor = MapExtractor(OneString, "str")
      extractor(Map("str" -> "hi there")) mustEqual OneString("hi there")
    }

    "boolean" in {
      map.extract[OneBool](Map("a_bool" -> true)) mustEqual OneBool(true)
      map.extract[OneBool](Map("a_bool" -> "foo")) must throwA[TypeMismatchException]
      map.extract[OneBool](Map("foo" -> true)) must throwA(new NoSuchElementException("key not found: a_bool"))
    }

    "char" in {
      val extractor = MapExtractor(OneChar, "char")
      extractor(Map("char" -> 30)) mustEqual OneChar(30)
      extractor(Map("char" -> 0)) mustEqual OneChar(0)
      extractor(Map("char" -> "foo")) must throwA[TypeMismatchException]
    }

    "byte" in {
      val extractor = MapExtractor(OneByte, "byte")
      extractor(Map("byte" -> 30)) mustEqual OneByte(30)
      extractor(Map("byte" -> 0)) mustEqual OneByte(0)
      extractor(Map("byte" -> "foo")) must throwA[TypeMismatchException]
    }

    "short" in {
      val extractor = MapExtractor(OneShort, "short")
      extractor(Map("short" -> 30)) mustEqual OneShort(30)
      extractor(Map("short" -> 0)) mustEqual OneShort(0)
      extractor(Map("short" -> "foo")) must throwA[TypeMismatchException]
    }

    "int" in {
      val extractor = MapExtractor(OneInt, "int")
      extractor(Map("int" -> 30)) mustEqual OneInt(30)
      extractor(Map("int" -> 0)) mustEqual OneInt(0)
      extractor(Map("int" -> "foo")) must throwA[TypeMismatchException]
    }

    "long" in {
      val extractor = MapExtractor(OneLong, "long")
      extractor(Map("long" -> 30)) mustEqual OneLong(30)
      extractor(Map("long" -> 0)) mustEqual OneLong(0)
      extractor(Map("long" -> "foo")) must throwA[TypeMismatchException]
    }

    "double" in {
      val extractor = MapExtractor(OneDouble, "double")
      extractor(Map("double" -> 30)) mustEqual OneDouble(30)
      extractor(Map("double" -> 0)) mustEqual OneDouble(0)
      extractor(Map("double" -> "foo")) must throwA[TypeMismatchException]
    }

    "float" in {
      val extractor = MapExtractor(OneFloat, "float")
      extractor(Map("float" -> 30)) mustEqual OneFloat(30)
      extractor(Map("float" -> 0)) mustEqual OneFloat(0)
      extractor(Map("float" -> "foo")) must throwA[TypeMismatchException]
    }

    "option" in {
      val extractor = MapExtractor(OneOpt, "float")
      extractor(Map("float" -> 30)) mustEqual OneOpt(Some(30))
      extractor(Map("notfloat" -> 30)) mustEqual OneOpt(None)
      extractor(Map("float" -> "foo")) must throwA[TypeMismatchException]
    }

    "inner extractor" in {
      implicit val inner = MapExtractor(OneFloat, "inner_float")
      val outer = MapExtractor(MapAndLong, "a_map", "a_long")

      outer(Map("a_map" -> Map("inner_float" -> 1.0), "a_long" -> 2)) mustEqual MapAndLong(OneFloat(1), 2)
    }
  }
}


case class OneByteArray(v: Array[Byte])

object RowExtractorSpec extends Specification with JMocker with ClassMocker {
  import java.sql.ResultSet

  val resultSet = mock[ResultSet]

  val boolExtractor = RowExtractor(OneBool, "c1")
  val byteArrayExtractor = RowExtractor(OneByteArray, "c1")

  "unnested bool works" in {
    expect {
      one(resultSet).getBoolean("c1") willReturn true
      one(resultSet).wasNull          willReturn false
    }

    boolExtractor(resultSet) mustEqual OneBool(true)
  }

  "bytearray works" in {
    val bytes = Array(1,2,3).map(_.toByte)

    expect {
      one(resultSet).getBytes("c1") willReturn bytes
      one(resultSet).wasNull        willReturn false
    }

    byteArrayExtractor(resultSet) mustEqual OneByteArray(bytes)
  }
}


case class Structured(s: String, f: Double, i: Int, ol: OneLong)

object JsonExtractorSpec extends Specification {
    implicit val inner = JsonExtractor(OneLong, "a_long")
    val extractor = JsonExtractor(Structured, "a_string", "a_double", "an_int", "an_object")

  "works" in {
    val json = """{ "a_string" : "foo", "an_int" : 1, "a_double" : 2.3, "an_object" : { "a_long" : 23 } }"""

    extractor(json) mustEqual Structured("foo", 2.3, 1, OneLong(23L))
  }
}
