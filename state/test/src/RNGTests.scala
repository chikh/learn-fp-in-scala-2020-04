package state

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
      multipleAsserts(nonNegativeInt, (r: Int) => r >= 0, 424242)
    }

    test("double") {
      multipleAsserts(double, (r: Double) => r >= 0 && r <= 1, 424242)
    }

    test("ints") {
      val (r, _) = ints(3)(SimpleRNG(42L))
      assert(r == List(16159453, -1281479697, -340305902))
      r
    }
  }

  def multipleAsserts[A](f: RNG => (A, RNG), condition: A => Boolean, n: Int): Unit = {
      @tailrec
      def go(times: Int, rng: RNG): Unit = {
        if (times > 0) {
          val (r, nextRng) = f(rng)
          assert(condition(r) == true)
          go(times - 1, nextRng)
        }
      }

      go(n, SimpleRNG(42L/*System.currentTimeMillis() will maybe fail the test in some cases :) */))
  }
}
