sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Maximum {
    def maximum(t: Tree[Int]): Int = {
        t match {
            case Leaf(v) => v
            case Branch(l, r) => maximum(l) max maximum(r)
        }
    }

    def main(args: Array[String]) = {
        val leaf_1 = Leaf[Int](12)
        val leaf_2 = Leaf[Int](9)
        val leaf_3 = Leaf[Int](22)
        val leaf_4 = Leaf[Int](3)
        val branch_1 = Branch[Int](leaf_1, leaf_2)
        val branch_2 = Branch[Int](leaf_3, leaf_4)
        val branch_3 = Branch[Int](branch_1, branch_2)
        println(maximum(branch_3))
    }
}