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
      var r = depth(
        Branch(
          Branch(Leaf(1), Leaf(2)),
          Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
        )
      )
      assert(r == 3)
      r
    }

    test("map") {
      var r = map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_.toString)
      assert(r == Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")))
      r
    }

    test("fold") {
      var r = fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(identity)(_ + _)
      assert(r == 6)
      r
    }
  }
}
