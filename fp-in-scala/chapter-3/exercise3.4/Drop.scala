object Drop {
    def drop[A](list: List[A], n: Int): List[A] = {
        if (n <= 0) list
        else list match {
            case x :: xs => drop(xs, n-1)
            case Nil => Nil
        }
    }

    def main(args: Array[String]) = {
        println(drop(List(1,2,3,4,5), 3))
        println(drop(List(1,2,3,4,5), 6))
        println(drop(List(1), 1))
        println(drop(List(1), 2))
        println(drop(List(), 2))
    }
}