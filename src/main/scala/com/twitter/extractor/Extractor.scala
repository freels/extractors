package com.twitter.extractor

class ExtractionException(desc: String, cause: Throwable) extends Exception(desc, cause) {
  def this(desc: String) = this(desc, null)
}

class TypeMismatchException(desc: String, cause: Throwable) extends ExtractionException(desc, cause)
class NoElementException(desc: String) extends ExtractionException(desc, null)

trait ValExtractor[Container, Key, Result] extends (Key => Container => Result) {
  protected def error(description: String): Nothing = throw new ExtractionException(description)
  protected def typeMismatch(key: String, cause: Throwable): Nothing =
    throw new TypeMismatchException("element at \"" + key + "\" is not the expected type.", cause)
  protected def noElement(key: String): Nothing = throw new NoElementException("element does not exist: \"" + key + "\"")
}

trait Extractor {
  type Container
  type Key
  type VE[T] <: ValExtractor[Container, Key, T]

  // def apply[Result, T1](constructor: (T1) => Result, c1: Key)(implicit ve1: VE[T1]) =
  //   new Extractor1(constructor, ve1(c1))

  // class Extractor1[Result, T1](constructor: (T1) => Result, c1: (Container => T1)) {
  //   def apply(c: Container) = constructor(c1(c))
  // }

  <#list 1..22 as i>

  <#assign paramTypes><#list 1..i as j>T${j}<#if i != j>,</#if> </#list></#assign>
  <#assign params><#list 1..i as j>c${j}: Key<#if i != j>, </#if></#list></#assign>
  <#assign implicitParams><#list 1..i as j>ve${j}: VE[T${j}]<#if i != j>, </#if></#list></#assign>
  <#assign classArgs><#list 1..i as j>ve${j}(c${j})<#if i != j>, </#if></#list></#assign>

  def apply[Result, ${paramTypes}](constructor: (${paramTypes}) => Result, ${params})(implicit ${implicitParams}) =
    new Extractor${i}(constructor, ${classArgs})

  <#assign applyArgs><#list 1..i as j>ve${j}(c${j})<#if i != j>, </#if></#list></#assign>
  <#assign classParams><#list 1..i as j>c${j}: (Container => T${j})<#if i != j>, </#if></#list></#assign>
  <#assign constructorArgs><#list 1..i as j>c${j}(c)<#if i != j>, </#if></#list></#assign>

  class Extractor${i}[Result, ${paramTypes}](constructor: (${paramTypes}) => Result, ${classParams}) {
    def apply(c: Container) = constructor(${constructorArgs})
  }

  </#list>
}
