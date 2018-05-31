package lambdaconf.introfp

object type_constructor {
  case class Person(name: String)

  object Person {
    //data constructor construct data
    def apply(name: String): Person = new Person(name)
  }

  val joh = Person("John")


  //type constructor

  /*
    String is a type
    List is not a type, it's a type constructor

    What values in this type?
    - List : question doesn't make sense
    - List[Int]: Nil, List(1,2,3) and so on


    v: List[Int] ------ List: Type => Type
    f(0) ------ List[String] (Int => String)


    List * => *
    Map *,* => *

    kind should return a type, not a type constructor

   */



   // Stack MUST have kind: * => *
   trait  StackLike[Stack[_]] { // StackLike has kind (* => *) => *
    def push[A](s: Stack[A], a: A): Stack[A]

    def pop[A](s: Stack[A]): Option[(A, Stack[A])]
   }

   //StackList[List]

   //StackList[String] //not valid

   //StackList[Function0] //valid

  
   // trait Function0[+R] // * => *
   // trait Map[K, V]     // [*, *] => * // (*, *) -> *
   // trait String

   val StackLikeList: StackLike[List] = new StackLike[List] {
    def push[A](s: List[A], a: A): List[A] = ???

    def pop[A](s: List[A]): Option[(A, List[A])] = ???
   }

  // [*, *] => * // F[_, _]

  // A : *
  // A[_, _]: [*, *] => *
  // A[_[_]]: (* => *) => *
  // A[S] where S = _[_] : (* => *) => *

   /*
    
    A[_[_, _], _[_]] 

    A[K, V] : [???, ???] => *
    _[_, _] : [*, *] => *
    _[_]    : * => *
    A[K, V] : [[*, *] => *, * => *] => *
  
    */

  trait Algorithm[MapLike[_, _], ListLike[_]] {}

  //Algorithm[Map, List]


  /*

    A[_[_[_]], _, _, [_, _, _]] : [(* => *) => *, *, [*, *, *] => *] => *

    //(* => *) => * is like StackLike, not necessarily Stack

    A[StackLike, String, Tuple3]

   */

  /*
    
    [*, *] => * // A[_, _]

    *                          // A

    (* => *) => *              // A[_[_]]
    ((* => *) => *) => *       // A[_[_[_]]]

    [[*, *] => *, * => *] => * // A[_[_, _] , _[_]]

    [((* => *) => *) => *, * => *] => *  // A[_[_[_[_]], _[_]]

  
   */


}
