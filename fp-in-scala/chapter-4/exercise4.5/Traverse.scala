object Traverse {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap (aa => b map(bb => f(aa, bb)))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
  
  def toInt(s: String): Option[Int] = {
      try {
          Some(Integer.parseInt(s.trim))
      } catch {
          case e: Exception => None
      }
  }

  def main(args: Array[String]) = {
    val arg1 = List("1", "2", "3", "5")
    val obj = traverse (arg1) (toInt)
    println(obj)
  }
}
