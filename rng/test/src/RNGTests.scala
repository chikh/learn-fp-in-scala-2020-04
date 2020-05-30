package rng

import utest._
import RNG._
import scala.annotation.tailrec

object RNGTests extends TestSuite {
  override def tests: Tests = Tests {
    test("SimpleRNG generates reproducible random number") {
      (0 to 4242).foreach { _ =>
        val (r, _) = SimpleRNG(42L).next
        assert(r == 16159453)
      }
    }

    test("non negative RNG") {
      @tailrec
      def go(times: Int, rng: RNG[Int]): Unit = {
        if (times > 0) {
          val (r, nextRng) = nonNegativeInt(rng)
          assert((r >= 2) == true)
          go(times - 1, nextRng)
        }
      }

      go(424242, SimpleRNG(42L/*System.currentTimeMillis() will maybe sometimes will fail the test :) */))
    }
  }
}
