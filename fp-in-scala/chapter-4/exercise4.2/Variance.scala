object Variance {
    def mean(xs: Seq[Double]): Option[Double] = {
        if (xs.isEmpty) None
        else Some(xs.sum/xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def main(args: Array[String]) = {
      println(variance(List(0.1, 1.0, 10.0, 3.0)))
      println(variance(List()))
    }
}
