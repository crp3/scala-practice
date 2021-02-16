object Filter {
    def filter[A](list: List[A])(f: A => Boolean): List[A] = {
        list match {
            case Nil => Nil
            case head :: tail => if(f(head)) head :: filter(tail)(f) else filter(tail)(f)
        }
    }

    def main(args: Array[String]) = {
        val list = List(1,2,3,4,5)
        println(filter(list)(x => x < 4))
        println(filter(list)(x => x % 2 == 0))
    }
}