
object type_lambda {
// type lambdas
// lambda is anonymous function

  trait CollectionLike[F[_]] {
    def foldLeft[A, Z](fa: F[A])(z: Z)(f: (Z, A) => Z): Z
  }

  val ListCollectionLike = new CollectionLike[List] {
    def foldLeft[A, Z](fa: List[A])(z: Z)(f: (Z, A) => Z): Z = fa.foldLeft(z)(f)
  }

  // Map[K, V] has kind problem; use partial application to handle it

  def plus(x: Int, y: Int) : Int = x + y
  val list = 1 :: 2 :: 3 :: Nil
  list.map(plus(1, _))

  // Partial Kind Application *********** !important

  def MapValueCollectionLike[K] = new CollectionLike[Map[K, ?]] { // ? kind projector
    // CollectionLike[Map[K, _]] doesn't exist in scala 2 yet, will be in 3
    def foldLeft[A, Z](fa: Map[K, A])(z: Z)(f: (Z, A) => Z): Z = fa.values.foldLeft(z)(f)
  }

  def MapValueCollectionLike2[K] = {
      type MapK[A] = Map[K, A]
      new CollectionLike[MapK] {
      def foldLeft[A, Z](fa: Map[K, A])(z: Z)(f: (Z, A) => Z): Z = fa.values.foldLeft(z)(f)
    }
  }

  def MapKeyCollectionLike[V] = new CollectionLike[Map[? , V]] {
    def foldLeft[A, Z](fa: Map[A, V])(z: Z)(f: (Z, A) => Z): Z = fa.keys.foldLeft(z)(f)
  }

  // val f = {def method: Int; type L } // structural type, this kind is not used in scala

  def MapKeyCollectionLike2[V] = new CollectionLike[({type L[K] = Map[K, V]})#L] {
    // L has kind * => *
    // this is what kind projector does
    def foldLeft[A, Z](fa: Map[A, V])(z: Z)(f: (Z, A) => Z): Z = fa.keys.foldLeft(z)(f)
  }

  //Tuple2 [*, *] => *

  def Tuple2CollectionLike1[B] = {
    type Tuple2B[X] = Tuple2[X, B]

    new CollectionLike[Tuple2B] {

      def foldLeft[A, Z](fa: Tuple2B[A])(z: Z)(f: (Z, A) => Z): Z = f(z, fa._1)
    }
  }

  def Tuple2CollectionLike2[B] = new CollectionLike[Tuple2[B, ?]] {
    def foldLeft[A, Z](fa: Tuple2[B, A])(z: Z)(f: (Z, A) => Z): Z = f(z, fa._2)
  }


  // Sized

  // * => *
  trait Sized[F[_]] {
    def size[A](fa: F[A]): Int
  }

  val ListSized = new Sized[List] {
    def size[A](fa: List[A]): Int = fa.length
  }

  // Map : [*, *] => *
  // F: * => *
  // Therefore cannot do Sized[Map] !
  def MapSized[K] = {

    type MapK[V] = Map[K, V]

    val x = new Sized[MapK] {
      def size[A](fa: MapK[A]): Int = fa.size
    }
  }

}