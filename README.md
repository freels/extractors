### An Example:

    scala> import com.twitter.extractor.MapExtractor
    import com.twitter.extractor.MapExtractor

    scala> case class Person(firstName: String, lastName: String, mi: Option[String], age: Int)
    defined class Person

    scala> val personFromMap = MapExtractor(Person, "first", "last", "middle_initial", "age")
    personFromMap: com.twitter.extractor.MapExtractor.Extractor4[Person,String,String,Option[String],Int] = com.twitter.extractor.Extractor$Extractor4@58033d09

    scala> val person1Map = Map("first" -> "Alice", "last" -> "Smith", "middle_initial" -> "B.", "age" -> 35)
    person1Map: scala.collection.immutable.Map[java.lang.String,Any] = Map((first,Alice), (last,Smith), (middle_initial,B.), (age,35))

    scala> val person2Map = Map("first" -> "Bob", "last" -> "Jones", "age" -> 22)
    person2Map: scala.collection.immutable.Map[java.lang.String,Any] = Map((first,Bob), (last,Jones), (age,22))

    scala> personFromMap(person1Map)
    res0: Person = Person(Alice,Smith,Some(B.),35)

    scala> personFromMap(person2Map)
    res1: Person = Person(Bob,Jones,None,22)

