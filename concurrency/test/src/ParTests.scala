package concurrency

import utest._
import Par._

object ParTests extends TestSuite {
  def tests = Tests {
    test("parallel sum") {
      test("the result is correct") {
        val r = run(sum(Vector(1, 2, 3, 4, 5)))
        assert(r == 15)
        r
      }
    }

    test("sequence") {
      val r = run(sequence(List(InThisThread(42), InNewThread(() => 44))))
      assert(r == List(42, 44))
      r
    }
  }
}
