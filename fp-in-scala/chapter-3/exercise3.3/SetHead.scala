object SetHead {
    def setHead[A](list: List[A], element: A): List[A] = {
        list match {
            case head :: tail => List(element) ++ tail
            case Nil => Nil
            case _ => List(element)
        }
    }

    def main(args: Array[String]) = {
        println(setHead(List(1,2,3,4,5), 99))
        println(setHead(List(), 99))
        println(setHead(List(1), 99))
    }
}