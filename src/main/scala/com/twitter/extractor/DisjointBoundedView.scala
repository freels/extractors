package com.twitter.extractor

// stolen from http://cleverlytitled.blogspot.com/2009/03/disjoint-bounded-views-redux.html

object DisjointBoundedView {
  type or[A,B] = Either[A,B]

  implicit def l[T](t: T) = Left(t)
  implicit def r[T](t: T) = Right(t)

  <#list 1..9 as i>
  <#assign ls><#list 1..i as j>l</#list></#assign>
  <#assign lefts><#list 1..i as j>Left(</#list></#assign>
  <#assign rparens><#list 1..i as j>)</#list></#assign>

  implicit def ${ls}l[T](t: T) = ${lefts}Left(t)${rparens}
  implicit def ${ls}r[T](t: T) = ${lefts}Right(t)${rparens}
  </#list>
}
