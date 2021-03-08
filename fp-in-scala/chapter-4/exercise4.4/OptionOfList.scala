object OptionOfList {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
  }

  def main(args: Array[String]) = {
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), Some(3), None, Some(4))))
  }
}
