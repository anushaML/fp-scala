package lambdaconf.introfp

/*
  product a tuple (String, Int) - geometric graph x/y  |_
    - multi dimension - tuple (A, B, C, ... )
  sum is a either [String, Int] - geometric flat line graph _ _
   - multi dimension - sealed trait and case class extending it
*/

object types {
  object products {

    // isomorphism
    // person and person2 are isomorphic
    // they can be translated to and from each other
    case class Person(name: String, age: Int)

    type Person2 = (String, Int)

    def personToPerson2(person: Person): Person2 = (person.name, person.age)

    def person2ToPerson(t: Person2): Person = Person(t._1, t._2)
  }

  object sums {
    sealed trait Lunch
    final case class Vegetarian(descr: String) extends Lunch
    final case class Paleo(descr: String) extends Lunch
    final case object Normal extends Lunch

    val v: Lunch = ???

    v match {
      case  Vegetarian(_) =>
      case Paleo(_) =>
      case Normal => 

    }
  }

  object exercises {
    case class Foo() // is a sum type, degenerate sum; sum of 1 term || is a degenerate product

    //isomorphic of String
    type String2 = (String, Unit)

    type String3 = Either[Nothing, String]

    def string3ToString(s: String3): String = s match {
      case Left(n) => n // Dead code, compiler knows this can't happen, because you can't do string3ToString(Left(???)), nothing to pass in 
      case Right(s) => s
    }

    // final cannot be extended, abstract so can't implement
    final abstract class Void { def absurd[A]: A } 
    type String4 = Either[Void, String]

    def string4ToString(s: String4): String = s match {
      case Left(v) => v.absurd[String] // again dead code
      case Right(s) => s
    }
  }
}

