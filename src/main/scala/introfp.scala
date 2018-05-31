package lambdaconf.introfp

object functions {
  object totality {
    /*
      domain and codomain

      types are sets of values

      every point in domain has to map to some value in codomain
      many to one is totality, but one to many means non-totality

      can have unmapped values in the codomain but not the domain

     */

    def notTotal1(s: String): Int = s.toInt
    def total2(s: Int): String = s.toString
    def nonTotal3(s: String): String = null
    def total4(s: String): Unit = () 
    def nonTotal5(s:String): Nothing = ??? // Nothing is empty set {}
  }

  object determinism {
    /*
      the input at any given time should produce same output
    */

    def notDeterministic1: Long = System.nanoTime()
    def det3(x: Int): Int = x*x 

    def deterministic3(x: Int): Int = {
      try println("hW") catch {case _ : Throwable => }

      x * x
    }

  }

  object purity {
    /*
       without side effects / not doing anything on the side

       not talking to external systems
       log statements
       database calls
       network / IO  interaction
    */

    def impure: Unit = println("Hello World")

    def pure: String = "Hello World"


  }
 
  /*

    ONE BENEFIT 

    all 3 give us equational reasoning 
    helps us understand fp easily 

    F(x) = x^2
    y = F(z)
    w = y + y

    fp , the one before = is just a name to one on right
    procedural programming, it's copying bits, not just naming


    this allows for "dumb" substitution
    w = y + y
      = F(z) + F(z)
      = z^2 + z^2
      = 2z^2


    TWO BENEFIT

    type based reasoning

  */

  object functions {

      //domain: set of all int values
      //codomain: "
      def sq (x: Int): Int = x * x //monomorphic function


  }

  /*
     Function that takes or returns a function or both

     example "map" takes function

     func.compose takes and returns function

     Function combinator = function that takes and returns function

   */
  object higher_order { 

    type Error = String

    type Parser[A] = String => Either[Error, (String, A)]

    def or[A](l: Parser[A], r: Parser[A]): Parser[A] = { (input: String) =>
      l(input) match {
        case Left(error) => r(input)
        case v => v
      }
    }

  }

  /*
    method doesn't know concrete type

    scala doesn't support polymorphism functions, but does for methods

    methods => def
    functions => val f : Int => String

    can pass functions around, not methods

    if you pass a method to where a function is needed, you need to specify types; 
    scala will then convert the methods to functions with the types
  */
  object polymorphism {

    //cannot do
    //val identity : A = ???

    // workaround 1
    def identity1[A](a: A): A = a

    // workaround 2

    // trait and class so you can easily pass around
    // ex: 
    // def test(i: Identity)
    // instead of 
    // def test(i: identity.type)

    trait Identity {

      def apply[A](a: A): A = a // parameteric polymorphism

    }

    object identity extends Identity

    identity(1) // 1
    identity("foo") // "foo"

    identity1(1)

    trait First {
      def apply[A, B](t: (A, B)): A = t._1
    }

    object first extends First

    trait Second {
      def apply[A, B](t: (A, B)): B = t._2
    }

    object second extends Second

  }

  // parameteric polymorphism is used a lot in scala 
  // subtype polymorphism is not used much is scala

  // scalazzi -- subset of scala used in this class
  // https://github.com/scalaz/scalazzi

  // 2 : Int
  // : means member of 

  
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


}
