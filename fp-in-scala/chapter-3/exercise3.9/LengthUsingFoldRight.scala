object LengthUsingFoldRight {

    def foldRight[A, B](list: List[A], element: B)(f: (A, B) => B): B = {
        list match {
            case Nil => element
            case x :: xs => f(x, foldRight(xs, element)(f))
        }
    }

    def length[A](list: List[A]): Int = {
        foldRight(list, 0)((_, acc) => acc + 1)
    }

    def main(args: Array[String]) = {
        println(length(List(1,2,3,4,5)))
    }
}