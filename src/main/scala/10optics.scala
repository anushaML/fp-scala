object optics { 

  // Optic[S, T, A, B]
  // S and T outer circle, A and B point in circle, arrow from S to T circle
  // Optic [S, A]

  // 3 times
  // lens - focus on term in product type, like name
  // prism - focus on term in sum type
  // traversal 


  // optics compose, except setter and getter together
  // like compose 2 lens, compose lens and setter and so on

  case class Person(name: String, age: Int)

  // monocle libraries can auto generate this

  object Person {
    val _Name = Lens[Person, String](_.name, (name: String) => _.copy(name = name))
    val _Age = Lens[Person, Int](_.age, (age: Int) => _.copy(age = age))
  }

  case class Car(driver: Person)

  object Car {
    val _Person = Lens[Car, Person](_.driver, (person: Person) => _.copy(driver = person))
  }

  case class Lens[S, A](
    get: S => A,
    set: A => S => S
  ) { self =>
    def  >>> [B](that: Lens[A, B]): Lens[S, B] = 
      Lens[S, B](
        get = (s : S) => that.get(self.get(s)),
        set = (b : B) => (s : S) => self.set(that.set(b)(self.get(s)))(s)
      )

    def modify(f: A => A): S => S = 
      (s: S) => self.set(f(self.get(s)))(s)
  }



  def main(args : Array[String]): Unit = {

    val TestCar: Car = Car(Person("Bob", 30))

    import Car._
    import Person._

    (_Person >>> _Age) : Lens[Car, Int]

    println((_Person >>> _Age).modify(_ + 10)(TestCar))


  }

}