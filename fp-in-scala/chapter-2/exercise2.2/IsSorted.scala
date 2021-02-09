object IsSorted {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int, assertion: Boolean): Boolean = 
            if (n+1 == as.length-1 && ordered(as(n), as(n+1))) assertion
            else loop(n+1, assertion && ordered(as(n), as(n+1)))
        loop(0, true)
    }

    def main(arguments: Array[String]): Unit = {
        val anonCompareInt = (x: Int, y: Int) => x <= y
        println(isSorted(Array(1,2,3,4), anonCompareInt))
        println(isSorted(Array(9,2,3,4), anonCompareInt))
    }
}