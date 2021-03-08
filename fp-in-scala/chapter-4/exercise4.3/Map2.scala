object Map2 { 
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap (aa => b map(bb => f(aa, bb)))
  }

  def main(args: Array[String]): Unit = {
    val c = Some(1)
    val c2 = Some(" hello")
    println(map2(c, c2) (_ + _))
  }
}
