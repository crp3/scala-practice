object GetTail {
    def getTail[A](list: List[A]): List[A] = {
        list match {
            case head :: tail => tail
            case Nil => Nil
            case _ => Nil
        }
    }

    def main(args: Array[String]) = {
        println(getTail(List(1,2,3,4,5)))
        println(getTail(List()))
        println(getTail(List(1)))
    }
}