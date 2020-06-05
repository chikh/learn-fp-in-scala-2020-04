package state

import utest._
import RNG._
import scala.annotation.tailrec

object RNGTests extends TestSuite {
  override def tests: Tests = Tests {
    test("SimpleRNG generates reproducible random number") {
      (0 to 4242).foreach { _ =>
        val (r, _) = SimpleRNG(42L).nextInt
        assert(r == 16159453)
      }
    }

    test("non negative RNG") {
      multipleAsserts(nonNegativeInt, (r: Int) => r >= 0, 424242)
    }

    test("double") {
      test("between 0 and 1") {
        multipleAsserts(double, (r: Double) => r >= 0 && r <= 1, 424242)
      }

      test("generates") {
        val (d, _) = double(SimpleRNG(42L))
        assert(d == 0.007524831689672932)
        d
      }
    }

    test("intDouble") {
      val (r, _) = intDouble(SimpleRNG(42L))
      assert(r == ((16159453, 0.5967354856416283)))
      r
    }

    test("ints") {
      val (r, _) = ints(3)(SimpleRNG(42L))
      assert(r == List(16159453, -1281479697, -340305902))
      r
    }

    test("sequence") {
      test("the same as ints") {
        val (r, _) = sequence(List(int, int, int))(SimpleRNG(42L))
        assert(r == List(16159453, -1281479697, -340305902))
        r
      }
    }
  }

  def multipleAsserts[A](
      f: RNG => (A, RNG),
      condition: A => Boolean,
      n: Int
  ): Unit = {
    @tailrec
    def go(times: Int, rng: RNG): Unit = {
      if (times > 0) {
        val (r, nextRng) = f(rng)
        assert(condition(r) == true)
        go(times - 1, nextRng)
      }
    }

    go(
      n,
      SimpleRNG(
        42L /*System.currentTimeMillis() will maybe fail the test in some cases :) */
      )
    )
  }
}
