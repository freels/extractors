## Extractors

Quick and easy extraction of structured data from unstructured data

### An Example

    scala> import com.twitter.extractors.json.JsonObjectExtractor

    scala> case class Person(firstName: String, lastName: String, mi: Option[String], age: Int)

    scala> val personFromJson = JsonObjectExtractor(Person, "first", "last", "middle_initial", "age")

    scala> personFromJson("""{ "first": "Alice", "last": "Smith", "middle_initial": "B", "age": 35 }""")
    res0: Person = Person(Alice,Smith,Some(B),35)

### Quick-and-dirty Usage

Currently, there are extractor factories for JSON, using Jackson, JDBC
ResultSets, and unstructured Map[String,Any].

To create an extractor function, you give your object factory function
as the first argument, followed by the keys for each factory
parameter, (generally strings):

    case class Person(firstName: String, lastName: String, mi: Option[String], age: Int)
    val personFromJson = JsonObjectExtractor(Person, "first", "last", "middle_initial", "age")

Based on the type signature of the factory function, converter
functions for each parameter are determined at compile time via
Scala's implicit parameter functionality. You will get a compile time
error if the type of a constructor parameter is unsupported by the
specific extractor.


### Nested Extractors

Additionally, extractors can be nested. In order for this to work, the
extractor should be an implicit value on the companion object of your
class:

    case class Child(name: String)

    object Child extends (String => Child) {
      implicit val fromJson = JsonObjectExtractor(apply, "name")
    }

    case class Parent(name: String, children: List[Child])

    object Parent extends ((String, List[Child]) => Parent) {
      implicit val fromJson = JsonObjectExtractor(apply, "name", "children")
    }


    scala> Parent.fromJson("""{ "name": "Dad", "children": [{"name": "Son"}] }""")
    res1: Parent(Dad,List(Child(Son)))


### Recursive Extractors

Extractors can created for recursive data types, however, it requires
adding an explicit type annotation, as the resulting extractor is a
recursive definition. (Not sure how to get around this.)


    case class IntCons(item: Int, next: Option[IntCons])

    object IntCons extends ((Int, Option[IntCons]) => IntCons) {
      implicit val fromJson: JsonObjectExtractor.Extractor[IntCons] =
        JsonObjectExtractor(IntCons, "item", "next")
    }


    scala> IntCons.fromJson("""{"item": 1, "next": {"item": 2, "next": { "item": 3 } } }""")
    res1: IntCons(1,Some(IntCons(2,Some(IntCons(3, None)))))
