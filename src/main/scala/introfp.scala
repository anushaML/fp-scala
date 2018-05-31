package lambdaconf.introfp

object functions {
  object totality {
    /*
      domain and codomain

      domain is type
      codomain is set of values

      every point in domain has to map to some value in codomain
      many to one is totality, but one to many means non-totality

     */

    def notTotal1(s: String): Int = s.toInt
    def total2(s: Int): String = s.toString
    def nonTotal3(s: String): String = null
    def total4(s: String): Unit = () 
    def nonTotal5(s:String): Nothing = ???
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
}
