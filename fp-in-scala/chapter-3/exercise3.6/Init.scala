object Init {
    def init[A](list: List[A]): List[A] = {
        list match {
            case Nil => Nil
            case _ :: Nil => Nil
            case x :: xs =>  List(x) ++ init(xs)
        }
    }

    def main(args: Array[String]) = {
        println(init(List(1,2,3,4,5)))
    }
}