package com.twitter.extractor

import org.specs.Specification

case class OneBool(v: Boolean)
case class OneChar(v: Char)
case class OneByte(v: Byte)
case class OneShort(v: Short)
case class OneInt(v: Int)
case class OneLong(v: Long)
case class OneDouble(v: Double)
case class OneFloat(v: Float)
case class OneString(v: String)
case class OneOpt(v: Option[Float])

object MapExtractorSpec extends Specification {
  "extracts" in {
    "object" in {
      val extractor = MapExtractor(OneString, "str" -> MVal[String])
      extractor(Map("str" -> "hi there")) mustEqual OneString("hi there")
    }

    "boolean" in {
      val extractor = MapExtractor(OneBool, "bool" -> MVal[Boolean])
      extractor(Map("bool" -> true)) mustEqual OneBool(true)
      extractor(Map("bool" -> 0)) mustEqual OneBool(false)
      extractor(Map("bool" -> 1)) mustEqual OneBool(true)
      extractor(Map("bool" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
      extractor(Map("foo" -> true)) must throwA(new NoElementException("element does not exist: \"bool\""))
    }

    "char" in {
      val extractor = MapExtractor(OneChar, "char" -> MVal[Char])
      extractor(Map("char" -> 30)) mustEqual OneChar(30)
      extractor(Map("char" -> 0)) mustEqual OneChar(0)
      extractor(Map("char" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "byte" in {
      val extractor = MapExtractor(OneByte, "byte" -> MVal[Byte])
      extractor(Map("byte" -> 30)) mustEqual OneByte(30)
      extractor(Map("byte" -> 0)) mustEqual OneByte(0)
      extractor(Map("byte" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "short" in {
      val extractor = MapExtractor(OneShort, "short" -> MVal[Short])
      extractor(Map("short" -> 30)) mustEqual OneShort(30)
      extractor(Map("short" -> 0)) mustEqual OneShort(0)
      extractor(Map("short" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "int" in {
      val extractor = MapExtractor(OneInt, "int" -> MVal[Int])
      extractor(Map("int" -> 30)) mustEqual OneInt(30)
      extractor(Map("int" -> 0)) mustEqual OneInt(0)
      extractor(Map("int" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "long" in {
      val extractor = MapExtractor(OneLong, "long" -> MVal[Long])
      extractor(Map("long" -> 30)) mustEqual OneLong(30)
      extractor(Map("long" -> 0)) mustEqual OneLong(0)
      extractor(Map("long" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "double" in {
      val extractor = MapExtractor(OneDouble, "double" -> MVal[Double])
      extractor(Map("double" -> 30)) mustEqual OneDouble(30)
      extractor(Map("double" -> 0)) mustEqual OneDouble(0)
      extractor(Map("double" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "float" in {
      val extractor = MapExtractor(OneFloat, "float" -> MVal[Float])
      extractor(Map("float" -> 30)) mustEqual OneFloat(30)
      extractor(Map("float" -> 0)) mustEqual OneFloat(0)
      extractor(Map("float" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }

    "option" in {
      val extractor = MapExtractor(OneOpt, "float" -> MVal[Option[Float]])
      extractor(Map("float" -> 30)) mustEqual OneOpt(Some(30))
      extractor(Map("notfloat" -> 30)) mustEqual OneOpt(None)
      extractor(Map("float" -> "foo")) must throwA(new ExtractionException("ref is not an AnyVal"))
    }
  }
}
