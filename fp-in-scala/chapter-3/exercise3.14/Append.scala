object Append {
    @annotation.tailrec
    def foldLeft[A, B](list: List[A], element: B)(f: (B, A) => B): B = {
        list match {
            case Nil => element
            case x :: xs => foldLeft(xs, f(element, x))(f)
        }
    }

    def append[A](list1: List[A], list2: List[A]): List[A] = {
        foldLeft(list1, list2)((x, y) => y :: x)
    }

    def main(args: Array[String]) = {
        println(append(List(1,2,3,4), List(5,6,7,8)))
    }
}