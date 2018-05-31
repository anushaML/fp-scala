

object existentials {

  /*
    don't have to reconstruct big structures and make it costly to map over

    existential type

    streaming fwks use similar optimizations
   */
  
  trait FastMap[A] { self =>

    type A0
    val original : Seq[A0]

    val mapping: A0 => A

    def map[B](f: A => B): FastMap[B] = new FastMap[B] {
      type A0      = self.A0
      val original = self.original
      val mapping  = self.mapping.andThen(f) // only composing
    }

    def run: Seq[A] = original.map(mapping)
  }

  object FastMap {
    def apply[A](as: A*): FastMap[A] = new FastMap[A] {
      type A0 = A
      val original = Seq(as: _*)
      val mapping = identity(_)
    }
  }

  // imagine this has million elements
  val fastMap = FastMap(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) 

  // normally will do lot of operations
  // with existential type, cost is O(1)
  val fastMap1 = fastMap.map(_ + 1).map(_ * 10).map(_ - 10) 

  // this will apply all map operations at once, instead of iterating every time
  fastMap1.run 
  


}