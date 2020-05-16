package dataStructures

import utest._
import Tree._

object TreeTests extends TestSuite {
  def tests = Tests {
    test("size") {
      val r = size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))
      assert(r == 5)
      r
    }

    test("maximum") {
      val r = maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2)))
      assert(r == 3)
      r
    }

    test("depth") {
      val r = depth(
        Branch(
          Branch(Leaf(1), Leaf(2)),
          Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
        )
      )
      assert(r == 3)
      r
    }

    test("map") {
      val r = map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_.toString)
      assert(r == Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")))
      r
    }

    test("fold") {
      val r = fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(identity)(_ + _)
      assert(r == 6)
      r
    }
  }
}
