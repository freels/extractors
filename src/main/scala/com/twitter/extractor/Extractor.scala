package com.twitter.extractor

import scala.reflect.Manifest

<#assign maxArity = 22>

class ExtractionException(desc: String, cause: Throwable) extends Exception(desc, cause) {
  def this(desc: String) = this(desc, null)
}

class NoElementException(desc: String) extends ExtractionException(desc, null)

trait ExtractorVal[Bucket, Key, Result] extends ((Bucket, Key) => Result) {
  protected val valMatch = """^(boolean|char|byte|short|int|long|double|float)$""".r

  protected def error(description: String): Nothing = throw new ExtractionException(description)
  protected def noElement(key: String): Nothing = throw new NoElementException("element does not exist: \"" + key + "\"")
}

trait ManifestedType[T] {
  protected def resultType: Manifest[T]

  protected def resultTypeArguments = {
    val method =  resultType.getClass.getMethod("typeArguments")
    method.invoke(resultType).asInstanceOf[Array[Manifest[Nothing]]]
  }
}

trait OptionalType[T] extends ManifestedType[T] {
  protected lazy val resultTypeIsOption = resultType.erasure.toString == "class scala.Option"
  protected lazy val innerResultType = if ( resultTypeIsOption ) resultTypeArguments.first else resultType

  protected def wrapOptional[T](f: => T) = {
    try {
      if ( resultTypeIsOption ) Some(f) else f
    } catch {
      case e: NoElementException => if ( resultTypeIsOption ) None else throw e
    }
  }
}

trait Extractor[Bucket, Key] {

  <#list 1..maxArity as i>

  <#assign valTypes>
    <#list 1..i as j>V${j}<#if i != j>,</#if> </#list>
  </#assign>

  <#assign valParams>
    <#list 1..i as j>
    c${j}: (Key, ExtractorVal[Bucket, Key, V${j}])<#if i != j>,</#if>
    </#list>
  </#assign>

  <#assign valArgs>
    <#list 1..i as j>c${j}<#if i != j>,</#if> </#list>
  </#assign>

  def apply[Result, ${valTypes}](constructor: (${valTypes}) => Result, ${valParams}) =
    new Extractor#{i}(constructor, ${valArgs})

  </#list>
}

<#list 1..maxArity as i>

<#assign valTypes>
  <#list 1..i as j>V${j}<#if i != j>,</#if> </#list>
</#assign>

<#assign valParams>
  <#list 1..i as j>
    c${j}: (Key, ExtractorVal[Bucket, Key, V${j}])<#if i != j>,</#if>
  </#list>
</#assign>

<#assign valArgs>
  <#list 1..i as j>e(c${j})<#if i != j>,</#if> </#list>
</#assign>

class Extractor${i}[+Result, +Key, -Bucket, ${valTypes}](constructor: (${valTypes}) => Result, ${valParams}) {
  def apply(b: Bucket) = {
    def e[V](t: (Key, ExtractorVal[Bucket, Key, V])): V = t._2(b, t._1)
    constructor(${valArgs})
  }
}

</#list>
