object ZipWith {
    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
        (l1, l2) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
        }
    }

    def main(args: Array[String]) = {
        val l1 = List(1,2,3,4,5)
        val l2 = List(2,3,4,5,6)
        println(zipWith(l1, l2)((x,y) => List(x,y)))
    }
}