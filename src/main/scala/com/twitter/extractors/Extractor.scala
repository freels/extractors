package com.twitter.extractors


trait ExtractorFactory {
  type Root
  type Container
  type Key

  def liftRoot(r: Root): Container
  def getWithKey(k: Key, c: Container): Container

  trait Extractor[R] extends (Root => R) {
    def apply(r: Root) = extract(liftRoot(r))
    def extract(c: Container): R
  }

  trait ValExtractor[R] extends (Container => R) {
    def handleError(e: Throwable): R = throw e
  }

  class KeyedExtractor[T : ValExtractor](k: Key) extends ValExtractor[T] {
    val inner = implicitly[ValExtractor[T]]

    def apply(c: Container) = try {
      inner(getWithKey(k, c))
    } catch {
      case e => inner.handleError(e)
    }
  }

  class LiftedValExtractor[T : ValExtractor] extends ValExtractor[Option[T]] {
    val inner = implicitly[ValExtractor[T]]

    override def handleError(e: Throwable) = e match {
      case e: NoSuchElementException => None
      case _ => throw e
    }

    def apply(c: Container) = try {
      Some(inner(c))
    } catch {
      case e: NoSuchElementException => None
    }
  }

  implicit def liftedValExtractor[T : ValExtractor] = new LiftedValExtractor[T]


  // def apply[R, T1](constructor: (T1) => R, k1: Key)(implicit ve1: ValExtractor[T1]) = {
  //   new Extractor1(constructor, mkKeyedE(k1, ve1)))
  // }

  // class Extractor1[R, T1](constructor: (T1) => R, c1: ValExtractor[T1]) extends Extractor[R] {
  //   def apply(c: Container) = constructor(c1(c))
  // }

  private def mkKeyedE[T](k: Key, e: ValExtractor[T]) = new KeyedExtractor(k)(e)

  <#list 1..22 as i>

  <#assign paramTypes><#list 1..i as j>T${j}<#if i != j>,</#if> </#list></#assign>
  <#assign params><#list 1..i as j>k${j}: Key<#if i != j>, </#if></#list></#assign>
  <#assign implicitParams><#list 1..i as j>ve${j}: ValExtractor[T${j}]<#if i != j>, </#if></#list></#assign>
  <#assign classArgs><#list 1..i as j>mkKeyedE(k${j}, ve${j})<#if i != j>, </#if></#list></#assign>

  def apply[R, ${paramTypes}](constructor: (${paramTypes}) => R, ${params})(implicit ${implicitParams}) = {
    new Extractor${i}(constructor, ${classArgs})
  }

  <#assign applyArgs><#list 1..i as j>ve${j}(c${j})<#if i != j>, </#if></#list></#assign>
  <#assign classParams><#list 1..i as j>c${j}: ValExtractor[T${j}]<#if i != j>, </#if></#list></#assign>
  <#assign constructorArgs><#list 1..i as j>c${j}(c)<#if i != j>, </#if></#list></#assign>

  class Extractor${i}[R, ${paramTypes}](constructor: (${paramTypes}) => R, ${classParams}) extends Extractor[R] {
    def extract(c: Container) = constructor(${constructorArgs})
  }

  </#list>
}

// trait SeqLikeExtractors extends ExtractorFactory {
//   type Key = Int

//   // def apply[R, T1](constructor: (T1) => R)(implicit ve1: ValExtractor[T1]) = {
//   //   new Extractor1(constructor, ve1(1, _))
//   // }

//   <#list 1..22 as i>

//   <#assign paramTypes><#list 1..i as j>T${j}<#if i != j>,</#if> </#list></#assign>
//   <#assign params><#list 1..i as j>k${j}: Key<#if i != j>, </#if></#list></#assign>
//   <#assign implicitParams><#list 1..i as j>ve${j}: ValExtractor[T${j}]<#if i != j>, </#if></#list></#assign>
//   <#assign classArgs><#list 1..i as j>ve${j}(${j}, _)<#if i != j>, </#if></#list></#assign>

//   def apply[R, ${paramTypes}](constructor: (${paramTypes}) => R)(implicit ${implicitParams}) = {
//     new Extractor${i}(constructor, ${classArgs})
//   }

//   </#list>
// }

trait NestedExtractors extends ExtractorFactory {
  class ExtractorExtractor[R : Extractor] extends ValExtractor[R] {
    val extractor = implicitly[Extractor[R]]

    def apply(c: Container) = extractor.extract(c)
  }

  implicit def extractorExtractorVal[T : Extractor] = new ExtractorExtractor[T]
}
