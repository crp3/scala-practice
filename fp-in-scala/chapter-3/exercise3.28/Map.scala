sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Map {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
        t match {
            case Leaf(v) => Leaf(f(v))
            case Branch(l, r) => Branch(map(l)(f), map(r)(f)) 
        }
    }
    def main(args: Array[String]) = {
        val leaf_1 = Leaf[Int](12)
        val leaf_2 = Leaf[Int](9)
        val leaf_3 = Leaf[Int](22)
        val leaf_4 = Leaf[Int](3)
        val branch_1 = Branch[Int](leaf_1, leaf_2)
        val branch_2 = Branch[Int](leaf_3, leaf_4)
        val tree = Branch[Int](branch_1, branch_2)
        println(map(tree)(x => x + 2))
        println(map(tree)(x => x.toString() + " !"))
    }
}