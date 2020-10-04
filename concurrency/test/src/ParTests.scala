package concurrency

import utest._
import Par._
import java.util.concurrent.Executors

object ParTests extends TestSuite {
  def tests = Tests {
    test("parallel sum") {
      test("the result is correct") {
        val r = run(sum(Vector(1, 2, 3, 4, 5)))(Executors.newFixedThreadPool(Int.MaxValue))
        assert(r == 15)
        r
      }
    }

    test("sequence") {
      val r = run(sequence(List(lazyUnit(42),  lazyUnit(44))))(Executors.newFixedThreadPool(Int.MaxValue))
      assert(r == List(42, 44))
      r
    }
  }
}
