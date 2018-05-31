package lambdaconf.introfp

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