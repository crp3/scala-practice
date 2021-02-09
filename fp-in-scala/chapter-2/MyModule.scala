object MyModule {
    def abs(n: Int): Int = 
        if (n < 0) -n
        else n

    def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int = 
            if(n==1) acc
            else go(n-1, acc*n)
        go(n, 1)
    }

    def formatResult(name: String, n: Int, f: Int => Int): String = {
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }

    private def formatAbs(x: Int): String = {
        val name = "absolute value"
        formatResult(name, x, abs)
    }

    private def formatFactorial(n: Int): String = {
        val name = "factorial"
        formatResult(name, n, factorial)
    }

    def main(args: Array[String]): Unit =
        println(formatAbs(-42))
        println(formatFactorial(10))
}