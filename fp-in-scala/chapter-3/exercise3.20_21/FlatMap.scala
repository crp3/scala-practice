object FlatMap {
    def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] = {
        list match {
            case Nil => Nil
            case head :: tail => f(head) ++ flatMap(tail)(f)
        }
    }

    def filter[A](list: List[A])(f: A => Boolean): List[A] = {
        flatMap(list)(x => if(f(x)) List(x) else Nil )
    }

    def main(args: Array[String]) = {
        val list = List(1,2,3,4,5)
        println(flatMap(list)(x => List(x,x)))
        println(filter(list)(x => x < 4))
    }
}