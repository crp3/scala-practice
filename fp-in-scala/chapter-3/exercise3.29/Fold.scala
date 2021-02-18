sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Fold {
    def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
        tree match {
            case Leaf(v) => f(v)
            case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
        }
        
    }
    
    def sizeUsingFold[A](tree: Tree[A]): Int = {
        fold(tree)(x => 1)(1 + _ + _)
    }

    def maximumUsingFold(tree: Tree[Int]): Int = {
        fold(tree)(x => x)(_ max _)
    }

    def depthUsingFold[A](tree: Tree[A]): Int = {
        fold(tree)(x => 1)((x, y) => 1 + (x max y))
    }

    def mapUsingFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
        fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
    }

    def main(args: Array[String]) = {
        val leaf_1 = Leaf[Int](12)
        val leaf_2 = Leaf[Int](9)
        val leaf_3 = Leaf[Int](22)
        val leaf_4 = Leaf[Int](3)
        val branch_1 = Branch[Int](leaf_1, leaf_2)
        val branch_2 = Branch[Int](leaf_3, leaf_4)
        val tree = Branch[Int](branch_1, branch_2)
        println(sizeUsingFold(tree))
        println(maximumUsingFold(tree))
        println(depthUsingFold(tree))
        println(mapUsingFold(tree)(_.toString() + " !"))
    }
}