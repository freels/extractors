package com.twitter.extractors

import scala.collection.generic.CanBuild

trait ExtractorFactory {
  type Root
  type Container
  type Key

  def liftRoot(r: Root): Container
  def containerForKey(c: Container, k: Key): Container

  class Base[R](val extractor: Extractor[R]) extends (Root => R) {
    def apply(r: Root) = extractor(liftRoot(r))
  }

  trait Extractor[R] {
    def apply(r: Container): R
    def handleError(e: Throwable): R = throw e
  }

  trait KeyedExtractor[R] extends Extractor[R] {
    protected def readValue[T](c: Container, k: Key, extractor: Extractor[T]): T = {
      val param = try {
        containerForKey(c, k)
      } catch {
        case e: NoSuchElementException => return extractor.handleError(e)
      }

      extractor(param)
    }
  }


  // define arity 1 manually

  class KeyedExtractor1[R, T1 : Extractor](constructor: (T1) => R, k1: Key) extends KeyedExtractor[R] {
    val ve1 = implicitly[Extractor[T1]]

    def apply(c: Container) = {
      val p1 = readValue(c, k1, ve1)

      constructor(p1)
    }
  }

  def apply[R, T1 : Extractor](constructor: (T1) => R, k1: Key) = {
    new Base(new KeyedExtractor1(constructor, k1))
  }

  <#list 2..22 as i>

  <#assign evidencedTypes><#list 1..i as j>T${j} : Extractor<#if i != j>, </#if></#list></#assign>
  <#assign paramTypes><#list 1..i as j>T${j}<#if i != j>, </#if></#list></#assign>
  <#assign keyParams><#list 1..i as j>k${j}: Key<#if i != j>, </#if></#list></#assign>
  <#assign keyArgs><#list 1..i as j>k${j}<#if i != j>, </#if></#list></#assign>

  def apply[R, ${evidencedTypes}](constructor: (${paramTypes}) => R, ${keyParams}) = {
    new Base(new KeyedExtractor${i}(constructor, ${keyArgs}))
  }

  class KeyedExtractor${i}[R, ${evidencedTypes}](constructor: (${paramTypes}) => R, ${keyParams}) extends KeyedExtractor[R] {

    <#list 1..i as j>
    val ve${j} = implicitly[Extractor[T${j}]]
    </#list>

    def apply(c: Container) = {
      <#list 1..i as j>
      val p${j} = readValue(c, k${j}, ve${j})
      </#list>

      <#assign constructorArgs><#list 1..i as j>p${j}<#if i != j>, </#if></#list></#assign>
      constructor(${constructorArgs})
    }
  }
  </#list>


  class LiftedExtractor[R : Extractor] extends Extractor[Option[R]] {
    val inner = implicitly[Extractor[R]]

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

  implicit def liftedExtractor[R : Extractor] = new LiftedExtractor[R]
}

trait IterableExtractors extends ExtractorFactory {
  def foreachInContainer(c: Container)(f: Container => Unit)

  class IterableExtractor[R, CC[_]](
    implicit inner: Extractor[R],
    bf: CanBuild[R,CC[R]])
  extends Extractor[CC[R]] {

    def apply(c: Container) = {
      val b = bf()
      foreachInContainer(c) { x => b += inner(x) }
      b.result
    }
  }

  implicit def iterableExtractor[R, CC[_]](implicit i: Extractor[R], bf: CanBuild[R,CC[R]]) = {
    new IterableExtractor[R,CC]
  }
}

trait NestedExtractors extends ExtractorFactory {
  class NestedExtractor[R](rw: () => Base[R]) extends Extractor[R] {
    lazy val extractor = rw().extractor

    def apply(c: Container) = extractor(c)
  }

  // a level of indirection in order to inject laziness into implicit resolution
  implicit def lazyExtractor[R]()(implicit rw: Base[R]) = rw
  implicit def nestedExtractor[R](implicit rw: () => Base[R]) = new NestedExtractor[R](rw)
}
