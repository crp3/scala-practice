sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Depth {
    def depth[A](t: Tree[A]): Int = {
        t match {
            case Leaf(_) => 1
            case Branch(l, r) => 1 + (depth(l) max depth(r)) 
        }
    }

    def main(args: Array[String]) = {
        val leaf_1 = Leaf[Int](12)
        val leaf_2 = Leaf[Int](9)
        val leaf_3 = Leaf[Int](22)
        val leaf_4 = Leaf[Int](3)
        val leaf_5 = Leaf[Int](1)
        val leaf_6 = Leaf[Int](12)
        val leaf_7 = Leaf[Int](3123)
        val branch_1 = Branch[Int](leaf_6, leaf_7)
        val branch_2 = Branch[Int](leaf_5, branch_1)
        val branch_3 = Branch[Int](leaf_1, branch_2)
        val branch_4 = Branch[Int](leaf_2, branch_3)
        val branch_5 = Branch[Int](leaf_3, leaf_4)
        val tree = Branch[Int](branch_4, branch_5)
        println(depth(tree))
    }
}