object DropWhile {
    def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
        list match {
            case x :: xs if (f(x)) => dropWhile(xs, f)
            case _ => list
        }
    }

    def main(args: Array[String]) = {
        println(dropWhile(List(1,2,3,4,5), (x: Int) => x < 3))
    }
}