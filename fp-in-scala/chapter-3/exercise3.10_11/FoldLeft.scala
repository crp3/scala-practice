object FoldLeft {
    @annotation.tailrec
    def foldLeft[A, B](list: List[A], element: B)(f: (B, A) => B): B = {
        list match {
            case Nil => element
            case x :: xs => foldLeft(xs, f(element, x))(f)
        }
    }

    def sum(list: List[Int]): Int = {
        foldLeft(list, 0)(_ + _)
    }

    def product(list: List[Double]): Double = {
        foldLeft(list, 1.0)(_ * _)
    }

    def length[A](list: List[A]): Int = {
        foldLeft(list, 0)((acc, _) => acc + 1)
    }

    def main(args: Array[String]) = {
        println(sum(List(1,2,3,4,5)))
        println(product(List(1.0,2.0,3.0,4.0,5.0)))
        println(length(List(1,2,3,4,5)))
    }
}