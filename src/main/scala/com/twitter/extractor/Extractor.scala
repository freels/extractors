package com.twitter.extractor

<#assign maxArity = 22>

object Extractor {
  type Val[Bucket, Key, Result] = (Bucket, Key) => Result
  type ValTuple[A, B, C] = (A, Val[B,A,C])
}

trait Extractor[Bucket, Key] {

  <#list 1..maxArity as i>

  <#assign valTypes>
    <#list 1..i as j>V${j}<#if i != j>,</#if> </#list>
  </#assign>

  <#assign valParams>
    <#list 1..i as j>
    c${j}: Extractor.ValTuple[Key, Bucket, V${j}]<#if i != j>,</#if>
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
    c${j}: Extractor.ValTuple[Key, Bucket, V${j}]<#if i != j>,</#if>
  </#list>
</#assign>

<#assign valArgs>
  <#list 1..i as j>e(c${j})<#if i != j>,</#if> </#list>
</#assign>

class Extractor${i}[+Result, +Key, -Bucket, ${valTypes}](constructor: (${valTypes}) => Result, ${valParams}) {
  def apply(b: Bucket) = {
    def e[V](t: Extractor.ValTuple[Key, Bucket, V]): V = t._2(b, t._1)
    constructor(${valArgs})
  }
}

</#list>
