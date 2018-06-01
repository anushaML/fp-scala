object type_classes {

  def main(args: Array[String]): Unit = {

  // polymorphic types are way of getting rid of structure
  // 2 reasons why write polymorphic code: reuse code and more chances of getting things right


  //type classes are to add back structure; minimum amount of structure back to solve the problem

  def repeat[A](n : Int, a: A): A = ???

  //append : (A, A) => A

  //type class
  trait Semigroup[A] { // type class

    // need some guarantees/laws, if not there is no defined/generic behavior
    // Associativity Law: append(append(a1, a2), a3) == appent(a1, append(a2, a3)) 
   def append(l: A, r: A): A
  }

  // To best find implicits...
  // Put implicits in this object if you don't control it, like Int, Tuple2
  // Put implicits in companion object if you control it, like Person
  object Semigroup {

    // Add this apply method to just use Semigroup[A] instead of implicitly[Semigroup[A]]
    def apply[A](implicit S: Semigroup[A]): Semigroup[A] = S 

    // instance of type class
    implicit val IntAddtionSemigroup = new Semigroup[Int] {
      def append(l: Int, r: Int): Int = l + r
    }

    implicit def SemigroupTuple2[A, B](implicit A: Semigroup[A], B: Semigroup[B]): Semigroup[(A, B)] = 
      new Semigroup[(A, B)] {
        def append(l: (A, B), r: (A, B)): (A, B) =
          (A.append(l._1, r._1), B.append(l._2, r._2))
      }

    implicit val StringSemigroup = new Semigroup[String] {
      def append(l: String, r: String): String = l + r
    }

    implicit def ListSemigroup[A](implicit A: Semigroup[A]): Semigroup[List[A]] = {
      new Semigroup[List[A]] {
        def append(l: List[A], r: List[A]): List[A] = l ++ r
      }
    }

    implicit def MapSemigroup[K, V](implicit V: Semigroup[V]): Semigroup[Map[K, V]] = {

      new Semigroup[Map[K, V]] {
        def append(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
          m1.foldLeft(m2) {
            case (m, (k, v)) => m + (k -> m.get(k).map(V.append(_, v)).getOrElse(v))
          }
        }
      }
    }

  }

  def repeat1[A](n: Int, a: A)(implicit S: Semigroup[A]): A = {
    if (n <= 1) a
    else S.append(a, repeat1(n-1, a)(S))
  }


  def repeat2[A: Semigroup](n: Int, a: A): A = { // alternative syntax for implicit 
    if (n <= 1) a
    else implicitly[Semigroup[A]].append(a, repeat2(n-1, a))
  }

  repeat1(5, 1)
  repeat1(5, (1, 4))

  repeat2(5, 1)
  repeat2(5, (1, 4))

//  repeat(List(1, 2), List(3, 4))



  // scala implicitly uses <> as Semigroup syntax 
  implicit class SemigroupSyntax[A](l: A) {
    def <> (r: A)(implicit S: Semigroup[A]): A = S.append(l, r)

    def append(r: A)(implicit S: Semigroup[A]): A = S.append(l, r)
  }

  def repeat3[A: Semigroup](n: Int, a: A): A = {
    if (n <= 1) a
    else a.append(repeat2(n-1, a))
  }

  // or 

  def repeat4[A: Semigroup](n: Int, a: A): A = {
    if (n <= 1) a
    else a <> repeat2(n-1, a)
  }

  "Hello " <> "World"

  ("Hello ", "Goodbye") <> ("Goodbye ", "Hello")

  (("A", "B"), "C") <> (("C", "B"), "A")


  //HOMEWORK

  // define Semigroup for List and Map
  // List concat
  // Map ..? have to make it Associative

    println(repeat1(5, (1, 4)))

    println((("A", "B"), "C") <> (("C", "B"), "A"))

    println(List("A", "B", "C") <> List("C", "B", "A"))

    println(Map("A" -> 1, "B" -> 2, "C" -> 2) <> Map("C" -> 3, "A" -> 4))
    println(Map("A" -> 1, "B" -> 2) <> Map("C" -> 3, "A" -> 4, "B" -> 3))



  // Tomorrow
  // How to define the classes as kindered types

}}