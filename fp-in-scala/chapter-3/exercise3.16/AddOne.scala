object AddOne {
    def addOne(list: List[Int]): List[Int] = {
        list match {
            case Nil => Nil
            case x :: xs => x+1 :: addOne(xs)
        }
    }

    def main(args: Array[String]) = { 
        println(addOne(List(1,2,3,4,5)))
    }
}