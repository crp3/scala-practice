object Reverse {
    @annotation.tailrec
    def foldLeft[A, B](list: List[A], element: B)(f: (B, A) => B): B = {
        list match {
            case Nil => element
            case x :: xs => foldLeft(xs, f(element, x))(f)
        }
    }

    def reverse[A](list: List[A]): List[A] = {
        foldLeft(list, List[A]())((acc: List[A], curr: A) => curr :: acc)
    }

    def main(args: Array[String]) = {
        println(reverse(List(1,2,3)))
    }
}