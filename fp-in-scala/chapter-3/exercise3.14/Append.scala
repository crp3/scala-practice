object Append {
    def foldRight[A, B](list: List[A], element: B)(f: (A, B) => B): B = {
        list match {
            case Nil => element
            case x :: xs => f(x, foldRight(xs, element)(f))
        }
    }

    def append[A](list1: List[A], list2: List[A]): List[A] = {
        foldRight(list1, list2)((x, y) => x :: y)
    }

    def main(args: Array[String]) = {
        println(append(List(1,2,3,4), List(5,6,7,8)))
    }
}