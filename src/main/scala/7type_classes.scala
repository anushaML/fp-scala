object type_classes {

  // def main(args: Array[String]): Unit = {

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

    // another way to define same thing above
    // implicit def MapSemigroup2[K, V: Semigroup ]: Semigroup[Map[K, V]] = {

    //   new Semigroup[Map[K, V]] {
    //     def append(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
    //       m1.foldLeft(m2) {
    //         case (m, (k, v)) => m + (k -> m.get(k).map(_ <> v).getOrElse(v))
    //       }
    //     }
    //   }
    // }

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
    //extension methods
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


    trait Monoid[A] extends Semigroup[A] {
      // Must satify: 
      // Left Zero: append (zero, a) == a
      // Right Zero: append (a, zero) == a

      def zero: A
    }

    object Monoid {
      def apply[A](implicit M: Monoid[A]): Monoid[A] = M

      implicit val MonoidInt: Monoid[Int] = new Monoid[Int] {
        def append(l: Int, r: Int): Int = l <> r

        val zero: Int = 0
      }


      def zero[A: Monoid]: A = Monoid[A].zero

      zero[Int]
    }

    //  *************** Eq ***************
    // Functor 

    /*
      Option, List and Future are functors


      but Set is not because if map results in 2 of same values, then one would disappear...
      so the structure doesn't remain the same
    */


    object functor {

      /**
       * Identity Law    : fmap(identity)(fa) === fa
       * Composition Law : fmap(f)(fmap(g)(fa)) === fmap(f.compose(g))(fa)
       */
      trait Functor[F[_]] {
        def fmap[A, B](f: A => B): F[A] => F[B]
      }
      object Functor {
        def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

        implicit val OptionFunctor = new Functor[Option] {
          def fmap[A, B](f: A => B): Option[A] => Option[B] =
            (o: Option[A]) => o.map(f)
        }
        implicit val ListFunctor = new Functor[List] {
          def fmap[A, B](f: A => B): List[A] => List[B] =
            (l: List[A]) => l.map(f)
        }

        type Function[A, B] = A => B

        implicit def FunctionFuctor[A]: Functor[Function[A, ?]] = new Functor[Function[A, ?]] {
          def fmap[B, C](f: B => C): (A => B) => (A => C) = (g: (A => B)) => f.compose(g)
          // Also works
          // def fmap[B, C](f: B => C): Function[A, B] => Function[A, C] = (g: Function[A, B]) => f.compose(g)
        }

      }
      implicit class FunctorSyntax[F[_], A](fa: F[A]) {
        def fmap[B](f: A => B)(implicit F: Functor[F]): F[B] = F.fmap(f)(fa)
      }
      Functor[List].fmap[Int, String](_.toString)(List(1, 2, 3))

      List(1, 2, 3).fmap(_.toString)


      // functors have excellent composition values
      case class Compose[F[_], G[_], A](fga: F[G[A]]) // if F and G are funtors then F[G[A]] is too
      case class Product[F[_], G[_], A](l: F[A], r: G[A]) // same
      case class Coproduct[F[_], G[_], A](e: Either[F[A], G[A]]) // both are funtcors

    }

    println(Map("A" -> 1, "B" -> 2, "C" -> 2).map(_.toString))

    //F[A] is a recipe that will produce 0 or more A

    // ****************** Monad ***************
    // IdentityMonad

    // Can't batch api calls with Monad. Can with Apply (ap)
    // Monads are powerful, but can't batch/parallelize api calls with flatMap

} //}