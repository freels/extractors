package com.twitter.extractors


class ExtractionException(desc: String, cause: Throwable) extends Exception(desc, cause) {
  def this(desc: String) = this(desc, null)
}

class TypeMismatchException(desc: String, cause: Throwable) extends ExtractionException(desc, cause)
class NoElementException(desc: String) extends ExtractionException(desc, null)

package object exceptions {
  def error(description: String): Nothing = throw new ExtractionException(description)
  def typeMismatch(): Nothing =
    throw new TypeMismatchException("element is not the expected type.", null)
  def noElement(key: String): Nothing = throw new NoSuchElementException("key not found: " + key)
}
