package dataStructures

import utest._
import List._

object ListTests extends TestSuite {
  def tests = Tests {
    test("drop") {
      test("one makes empty") {
        val result = drop(Cons(1, Nil), 1)
        assert(result == Nil)
        result
      }

      test("from Nil throws") {
        intercept[NoSuchElementException] {
          drop(Nil, 1)
        }
      }
    }

    type FoldRight[A, B] = List[A] => B => ((A, B) => B) => B

    def foldRightSumTest(f: FoldRight[Int, Int]) = {
      val r = f(List(1, 2, 3))(0)(_ + _)
      assert(r == 6)
      r
    }

    def foldRightKeepsOrder(f: FoldRight[Int, List[Int]]) = {
      val r = f(List(1, 2, 3))(Nil)(Cons(_, _))
      assert(r == List(1, 2, 3))
      r
    }

    test("foldRight") {
      test("calcs the sum") {
        foldRightSumTest(foldRight)
      }

      test("keeps order") {
        foldRightKeepsOrder(foldRight)
      }
    }

    test("foldLeft") {
      val r = foldLeft(List(1, 2, 3))(0)(_ + _)
      assert(r == 6)
      r
    }

    test("foldRightViaLeft") {
      test("calcs the sum") {
        foldRightSumTest(foldRightViaLeft)
      }

      test("keeps order") {
        foldRightKeepsOrder(foldRightViaLeft)
      }
    }

    test("reverse") {
      val r = reverse(List(1, 2, 3))
      assert(r == List(3, 2, 1))
      r
    }
  }
}
