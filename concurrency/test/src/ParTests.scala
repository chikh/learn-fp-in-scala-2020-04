package concurrency

import utest._
import Par._

object ParTests extends TestSuite {
  def tests = Tests {
    test("sum in parallel") {
      val s = Vector(1, 2, 3, 4, 5)

      val r = parSum(s)

      test("result is the same as normal sum") {
        assert(r == s.sum)
        r
      }
    }
  }
}
