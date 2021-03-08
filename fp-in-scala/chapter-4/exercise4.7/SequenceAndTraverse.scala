object SequenceAndTraverse {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] ={
    as match {
      case Nil => Right(Nil)
      case x :: xs => (f(x) map2 traverse(xs)(_ :: _))
    }
  }


  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }
}
