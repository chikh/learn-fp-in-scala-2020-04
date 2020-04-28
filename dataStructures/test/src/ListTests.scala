package dataStructures

import utest._
import List._

object ListTests extends TestSuite {
  def tests = Tests {
    test("length") {
      val r = length(List(1, 2, 3))
      assert(r == 3)
      r
    }

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

    test("append") {
      val r = append(List(1, 2))(List(3, 4))
      assert(r == List(1, 2, 3, 4))
      r
    }

    test("concat") {
      test("should do simple concat") {
        val r = concat(List(List(1, 2), List(3, 4)))
        assert(r == List(1, 2, 3, 4))
        r
      }

      /* TODO test("should be linear to the amount of elements") { ??? } */
    }

    test("map") {
      val r = map(List(1, 2, 3))(_.toString)
      assert(r == List("1", "2", "3"))
      r
    }

    test("filter") {
      var r = filter(List(1, 2, 3, 4))(_ % 2 == 0)
      assert(r == List(2, 4))
      r
    }

    test("flatMap") {
      var r = flatMap(List(1, 2, 3))(i => List(i, i))
      assert(r == List(1, 1, 2, 2, 3, 3))
      r
    }

    test("zipWith") {
      test("general case") {
        val r = zipWith(List(1, 2))(List(3, 4))(_ + _)
        assert(r == List(4, 6))
        r
      }

      test("the result length should be first list length if it's shorter") {
        val first = List(1)
        val r = length(zipWith(first)(List(2, 3))(_ + _))
        assert(r == length(first))
        r
      }

      test("the result length should be second list length if it's shorter") {
        val second = List(1)
        val r = length(zipWith(List(2, 3))(second)(_ + _))
        assert(r == length(second))
        r
      }

      test("with Nil should be Nil") {
        var r = zipWith(Nil: List[Int])(Nil: List[Int])(_ + _)
        assert(r == Nil)
        r
      }
    }

    test("hasSubsequence") {
      test("true") {
        var r = hasSubsequence(List(1, 2, 3, 4), List(2, 3))
        assert(r == true)
        r
      }

      test("false") {
        var r = hasSubsequence(List(1, 2, 3, 4), List(1, 3))
        assert(r == false)
        r
      }
    }
  }
}
