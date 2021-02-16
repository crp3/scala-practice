object AddPairWise {
    def addPairWise[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = {
        (l1, l2) match {
            case (_, Nil) => Nil
            case (Nil, _) => Nil
            case (h1 :: t1, h2 :: t2) => f(h1, h2) :: addPairWise(t1, t2)(f) 
        }
    }

    def main(args: Array[String]) = {
        val l1 = List(1,2,3)
        val l2 = List(4,5,6)
        println(addPairWise(l1, l2)(_+_))
    }
}