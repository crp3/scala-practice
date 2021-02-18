sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Size {
    def size[A](t: Tree[A]): Int = {
        t match {
            case Branch(left, right) => 1 + size(left) + size(right)
            case Leaf(value) => 1
        }
    }

    def main(args: Array[String]) = {
        val leaf_1 = Leaf[String]("Lol")
        val leaf_2 = Leaf[String]("Lol")
        val leaf_3 = Leaf[String]("Lol")
        val leaf_4 = Leaf[String]("Lol")
        val branch_1 = Branch[String](leaf_1, leaf_2)
        val branch_2 = Branch[String](leaf_3, leaf_4)
        val tree = Branch[String](branch_1, branch_2)
        println(size(tree))
    }

}
