package com.twitter.extractor

class ExtractionException(desc: String, cause: Throwable) extends Exception(desc, cause) {
  def this(desc: String) = this(desc, null)
}

class TypeMismatchException(desc: String, cause: Throwable) extends ExtractionException(desc, cause)
class NoElementException(desc: String) extends ExtractionException(desc, null)


trait ExtractorFactory {
  type Container
  type Key

  trait Extractor[R] extends (Container => R)

  trait ValExtractor[R] extends PartialFunction[(Key, Container), R] {
    def apply(k: Key, c: Container): R
    def isDefinedAt(k: Key, c: Container): Boolean

    def apply(kc: (Key, Container)): R = apply(kc._1, kc._2)
    def isDefinedAt(kc: (Key, Container)): Boolean = isDefinedAt(kc._1, kc._2)


    protected def error(description: String): Nothing = throw new ExtractionException(description)
    protected def typeMismatch(key: String, cause: Throwable): Nothing =
      throw new TypeMismatchException("element at \"" + key + "\" is not the expected type.", cause)
    protected def noElement(key: String): Nothing = throw new NoSuchElementException("key not found: " + key)
  }

  class LiftedValExtractor[T : ValExtractor] extends ValExtractor[Option[T]] {
    def apply(k: Key, c: Container) = implicitly[ValExtractor[T]].lift((k, c))
    def isDefinedAt(k: Key, c: Container) = true
  }

  implicit def liftedValExtractor[T : ValExtractor] = new LiftedValExtractor[T]


  // def apply[R, T1](constructor: (T1) => R, k1: Key)(implicit ve1: ValExtractor[T1]) =
  //   new Extractor1(constructor, ve1(k1, _))

  // class Extractor1[R, T1](constructor: (T1) => R, c1: (Container => T1)) extends Extractor[R] {
  //   def apply(c: Container) = constructor(c1(c))
  // }

  <#list 1..22 as i>

  <#assign paramTypes><#list 1..i as j>T${j}<#if i != j>,</#if> </#list></#assign>
  <#assign params><#list 1..i as j>k${j}: Key<#if i != j>, </#if></#list></#assign>
  <#assign implicitParams><#list 1..i as j>ve${j}: ValExtractor[T${j}]<#if i != j>, </#if></#list></#assign>
  <#assign classArgs><#list 1..i as j>ve${j}(k${j}, _)<#if i != j>, </#if></#list></#assign>

  def apply[R, ${paramTypes}](constructor: (${paramTypes}) => R, ${params})(implicit ${implicitParams}) =
    new Extractor${i}(constructor, ${classArgs})

  <#assign applyArgs><#list 1..i as j>ve${j}(c${j})<#if i != j>, </#if></#list></#assign>
  <#assign classParams><#list 1..i as j>c${j}: (Container => T${j})<#if i != j>, </#if></#list></#assign>
  <#assign constructorArgs><#list 1..i as j>c${j}(c)<#if i != j>, </#if></#list></#assign>

  class Extractor${i}[R, ${paramTypes}](constructor: (${paramTypes}) => R, ${classParams}) extends Extractor[R] {
    def apply(c: Container) = constructor(${constructorArgs})
  }

  </#list>
}

trait NestedExtractors extends ExtractorFactory {
  def getFromContainer[R](k: Key, c: Container): R
  def containerIsDefinedAt(k: Key, c: Container): Boolean

  class ExtractorExtractor[E <: ExtractorFactory, R : E#Extractor] extends ValExtractor[R] {
    val extractor = implicitly[E#Extractor[R]]

    def apply(k: Key, c: Container)       = extractor(getFromContainer(k, c))
    def isDefinedAt(k: Key, c: Container) = containerIsDefinedAt(k, c)
  }

  implicit def extractorExtractorVal[E <: ExtractorFactory, T : E#Extractor] = new ExtractorExtractor[E,T]
}
