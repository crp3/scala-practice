object DoubleToString {
    def doubleToString(list: List[Double]): List[String] = {
        list match {
            case Nil => Nil
            case x :: xs => x.toString() :: doubleToString(xs)
        }
    }

    def main(args: Array[String]) = { 
        val processed = doubleToString((List(1.0,2.0,3.1,4.3,5.9)))
        println(processed(0) + " <- testando essa string")
    }
}