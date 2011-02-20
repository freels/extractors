package com.twitter.extractors


trait ExtractorFactory {
  type Container
  type Key

  trait Extractor[R] extends (Container => R)


  trait ValExtractor[R] extends ((Key, Container) => R) {
    def apply(k: Key, c: Container): R
  }

  class LiftedValExtractor[T : ValExtractor] extends ValExtractor[Option[T]] {
    val inner = implicitly[ValExtractor[T]]

    def apply(k: Key, c: Container) = try {
      Some(inner(k,c))
    } catch {
      case e: NoSuchElementException => None
    }
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
  def getFromContainer[R](e: Extractor[R], k: Key, c: Container): R

  class ExtractorExtractor[R : Extractor] extends ValExtractor[R] {
    val extractor = implicitly[Extractor[R]]

    def apply(k: Key, c: Container) = getFromContainer(extractor, k, c)
  }

  implicit def extractorExtractorVal[T : Extractor] = new ExtractorExtractor[T]
}
