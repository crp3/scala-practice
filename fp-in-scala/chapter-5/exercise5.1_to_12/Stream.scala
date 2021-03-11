object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n > 0) => Stream.cons(h(), t().take(n-1))
    case Cons(h, _) if(n == 0) => Stream.empty
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n > 0) => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  
  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a,b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
  }

  def forAllUsingFoldRight(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h,t) => 
    if (p(h)) Stream.cons(h, t)
    else Stream.empty)
  }

  def headOptionUsingFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, t) => Some(h))
  }

  def mapUsingFoldRight[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))
  }

  def filterUsingFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if(p(h)) Stream.cons(h, t) else t)
  }

  def appendUsingFoldRight[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => Stream.cons(h, t))
  }

  def flatMapUsingFoldRight[B>:A](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h) appendUsingFoldRight t)
  }
}

object Main {
  //Infinite Streams
  def constant[A](a: A): Stream[A] = {
    lazy val constants: Stream[A] = Cons(() => a, () => constants)
    constants
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  def fibs: Stream[Int] = {
    def go(n: Int, m: Int): Stream[Int] = {
      Stream.cons(n, go(m, n+m))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => Stream.cons(h, unfold(s)(f))
      case None => Stream.empty
    }
  }

  def constantUsingUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }

  def fromUsingUnfold(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x, x+1))
  }

  def fibsUsingUnfold: Stream[Int] = {
    unfold((0, 1)){ case (x, y) => Some((x,(y, x+y))) }
  }

  def main(args: Array[String]) = {
    val s = Stream(1, 2, 3, 4, 5)
    val s2 = Stream(6, 7, 8, 9, 10)
    println(s.toList)
    println(s.take(4).toList)
    println(s.drop(3).toList)
    println(s.takeWhile(_ * 2 < 8).toList)
    println(s.takeWhileUsingFoldRight(_ * 2 < 8).toList)
    println(s.exists(_*2 == 10))
    println(s.forAll(_ < 6))
    println(s.forAllUsingFoldRight(_ < 6))
    println(s.forAll(_ % 2 == 0))
    println(s.forAllUsingFoldRight(_ % 2 == 0))
    println(s.mapUsingFoldRight(_ * 2).toList)
    println(s.filterUsingFoldRight(_ * 2 < 9).toList)
    println(s.appendUsingFoldRight(s2).toList)
    println(s.flatMapUsingFoldRight(x => Stream(x, x, x)).toList)
    val ones = constant(1)
    val fromTest = from(100)
    val fibs_ = fibs
    println("Infinite streams from here")
    println(ones.take(100).toList)
    println(ones.forAll(_ != 1))
    println(fromTest.take(100).toList)
    println(fromTest.forAll(_ == 1))
    println(fibs.take(100).toList)
    val onse = constantUsingUnfold(1)
    val formTest = fromUsingUnfold(100)
    val fibs_2 = fibsUsingUnfold
    println(onse.take(100).toList)
    println(formTest.take(100).toList)
    println(fibs_2.take(100).toList)
  }
}
