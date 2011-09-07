package com.twitter.extractors

import scala.collection.generic.CanBuild
import com.twitter.extractors.exceptions._


trait ExtractorFactory {
  type Container

  trait Extractor[R] {
    def apply(c: Container): R

    def applyOption(c: Option[Container]) = c match {
      case Some(c) => apply(c)
      case None    => extractError()
    }

    def extractError(): R = {
      throw new ExtractionException("decode failed.")
    }
  }

  trait ProductExtractor[R] extends Extractor[R]

  // define arity 1 manually

  def apply[R, T1, K1](
    constructor: (T1) => R,
    k1: => K1
  )(
    implicit ke1: K1 => Extractor[T1]
  ) = {
    new ProductExtractor1(constructor, ke1(k1))
  }

  class ProductExtractor1[R, T1](
    constructor: (T1) => R,
    r1: => Extractor[T1])
  extends ProductExtractor[R] {

    lazy val e1 = r1

    def apply(c: Container) = {
      val p1 = e1(c)
      constructor(p1)
    }
  }

  <#list 2..22 as i>

  def apply[R, <#list 1..i as j>T${j}, K${j}<#if i != j>, </#if></#list>](
    constructor: (<#list 1..i as j>T${j}<#if i != j>, </#if></#list>) => R,
    <#list 1..i as j>k${j}: => K${j}<#if i != j>, </#if></#list>
  )(
    implicit <#list 1..i as j>ke${j}: K${j} => Extractor[T${j}]<#if i != j>, </#if></#list>
  ) = {
    new ProductExtractor${i}(
      constructor,
      <#list 1..i as j>ke${j}(k${j})<#if i != j>, </#if></#list>
    )
  }

  class ProductExtractor${i}[R, <#list 1..i as j>T${j}<#if i != j>, </#if></#list>](
    constructor: (<#list 1..i as j>T${j}<#if i != j>, </#if></#list>) => R,
    <#list 1..i as j>r${j}: => Extractor[T${j}]<#if i != j>, </#if></#list>)
  extends ProductExtractor[R] {

    <#list 1..i as j>
    lazy val e${j} = r${j}
    </#list>

    def apply(c: Container) = {
      <#list 1..i as j>
      val p${j} = e${j}(c)
      </#list>
      constructor(<#list 1..i as j>p${j}<#if i != j>, </#if></#list>)
    }
  }
  </#list>
}

trait LiftedExtractors extends ExtractorFactory {
  class LiftedExtractor[R](inner: Extractor[R]) extends Extractor[Option[R]] {
    override def extractError() = None

    def apply(c: Container) = Some(inner(c))
  }

  implicit def extractorToLifted[R](e: Extractor[R]): Extractor[Option[R]] = {
    new LiftedExtractor[R](e)
  }

  implicit def liftedExtractorForType[R : Extractor]: Extractor[Option[R]] = {
    extractorToLifted(implicitly[Extractor[R]])
  }
}

trait KeyExtractors extends ExtractorFactory {
  type Key

  def keyMapper(c: Container, k: Key): Option[Container]

  class KeyExtractor[R](key: Key, inner: Extractor[R]) extends Extractor[R] {
    def apply(c: Container) = inner.applyOption(keyMapper(c, key))
  }

  implicit def key[R : Extractor](k: Key): Extractor[R] = {
    new KeyExtractor(k, implicitly[Extractor[R]])
  }
}

trait IterableExtractors extends ExtractorFactory {

  def iterableMapper(c: Container): Iterable[Container]

  class IterableExtractor[R, CC[R]](inner: Extractor[R], bf: CanBuild[R,CC[R]])
  extends Extractor[CC[R]] {
    def apply(c: Container) = {
      val b = bf()
      iterableMapper(c) foreach { x => b += inner(x) }
      b.result
    }
  }

  implicit def iterableExtractor[R, CC[_]](implicit e: Extractor[R], bf: CanBuild[R,CC[R]]): Extractor[CC[R]] = {
    new IterableExtractor[R,CC](e, bf)
  }
}
