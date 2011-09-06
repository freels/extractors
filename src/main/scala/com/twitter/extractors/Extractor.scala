package com.twitter.extractors

import scala.collection.generic.CanBuild
import com.twitter.extractors.exceptions._

trait Extractor[C,R] {
  def apply(c: C): R

  def applyOption(c: Option[C]) = c match {
    case Some(c) => apply(c)
    case None    => extractError()
  }

  def extractError(): R = {
    throw new ExtractionException("decode failed.")
  }
}

object Extractor {
  type AbstractExtractor[C,R] = Extractor[C,R]
}

trait ExtractorFactory {
  type Container

  trait Extractor[R] extends Extractor.AbstractExtractor[Container,R]

  abstract class SubcontainerExtractor[R](inner: Extractor[R]) extends Extractor[R] {
    def apply(c: Container) = inner.applyOption(subcontainer(c))

    def subcontainer(c: Container): Option[Container]
  }

  class LiftedExtractor[R](inner: Extractor[R]) extends Extractor[Option[R]] {
    def apply(c: Container) = Some(inner(c))

    override def extractError() = None
  }

  implicit def liftedExtractor[R : Extractor]: Extractor[Option[R]] = {
    new LiftedExtractor[R](implicitly[Extractor[R]])
  }
}

trait KeyedExtractors extends ExtractorFactory {
  type Key

  def KeyExtractor[R](k: Key, e: Extractor[R]): Extractor[R]

  trait KeyedExtractor[R] extends Extractor[R]

  // define arity 1 manually

  class KeyedExtractor1[R, T1 : Extractor](constructor: (T1) => R, k1: Key) extends KeyedExtractor[R] {

    val ve1 = KeyExtractor(k1, implicitly[Extractor[T1]])

    def apply(c: Container) = {
      val p1 = ve1(c)
      constructor(p1)
    }
  }

  def apply[R, T1 : Extractor](constructor: (T1) => R, k1: Key) = {
    new KeyedExtractor1(constructor, k1)
  }

  <#list 2..22 as i>

  <#assign evidencedTypes><#list 1..i as j>T${j} : Extractor<#if i != j>, </#if></#list></#assign>
  <#assign paramTypes><#list 1..i as j>T${j}<#if i != j>, </#if></#list></#assign>
  <#assign keyParams><#list 1..i as j>k${j}: Key<#if i != j>, </#if></#list></#assign>
  <#assign keyArgs><#list 1..i as j>k${j}<#if i != j>, </#if></#list></#assign>

  def apply[R, ${evidencedTypes}](constructor: (${paramTypes}) => R, ${keyParams}) = {
    new KeyedExtractor${i}(constructor, ${keyArgs})
  }

  class KeyedExtractor${i}[R, ${evidencedTypes}](constructor: (${paramTypes}) => R, ${keyParams}) extends KeyedExtractor[R] {

    <#list 1..i as j>
    val ve${j} = KeyExtractor(k${j}, implicitly[Extractor[T${j}]])
    </#list>

    def apply(c: Container) = {
      <#list 1..i as j>
      val p${j} = ve${j}(c)
      </#list>
      constructor(<#list 1..i as j>p${j}<#if i != j>, </#if></#list>)
    }
  }
  </#list>
}

trait IterableExtractors extends ExtractorFactory {

  def IterableExtractor[R, CC[R]](inner: Extractor[R], bf: CanBuild[R,CC[R]]): Extractor[CC[R]]

  abstract class IterableExtractor[R, CC[R]](inner: Extractor[R], bf: CanBuild[R,CC[R]])
  extends Extractor[CC[R]] {

    def apply(c: Container) = {
      val b = bf()
      subcontainers(c) foreach { x => b += inner(x) }
      b.result
    }

    def subcontainers(c: Container): Iterable[Container]
  }

  implicit def iterableExtractor[R, CC[R]](implicit i: Extractor[R], bf: CanBuild[R,CC[R]]): Extractor[CC[R]] = {
    IterableExtractor(i, bf)
  }
}
