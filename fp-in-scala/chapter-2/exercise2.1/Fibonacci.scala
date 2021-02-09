object Fibonacci {
    def fib(n: Int): Int = { 
        def go(a: Int, b: Int, n: Int): Int = 
            if(n > 1) go(b, a+b, n-1)
            else b
        go(0, 1, n)
    }

    def main(args: Array[String]): Unit = {
        println(fib(10))
    }
}