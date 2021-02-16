object Map {
    def map[A, B](list: List[A])(f: A => B): List[B] = {
        list match {
            case Nil => Nil
            case head :: tail => f(head) :: map(tail)(f)
        }
    }
    
    def main(args: Array[String]) = {
        println(map(List(1,2,3,4,5))(x => x+1))
        println(map(List(1.0,2.1,3.0,5.5,4.0))(x => x.toString))
    }
}