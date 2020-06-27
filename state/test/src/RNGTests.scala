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
      multipleAsserts(nonNegativeInt.run, (r: Int) => r >= 0, 424242)
    }

    test("double") {
      test("between 0 and 1") {
        multipleAsserts(double.run, (r: Double) => r >= 0 && r <= 1, 424242)
      }

      test("generates") {
        val (d, _) = double.run(SimpleRNG(42L))
        assert(d == 0.007524831689672932)
        d
      }
    }

    test("intDouble") {
      val (r, _) = intDouble.run(SimpleRNG(42L))
      assert(r == ((16159453, 0.5967354856416283)))
      r
    }

    test("ints") {
      val (r, _) = ints(3).run(SimpleRNG(42L))
      assert(r == List(16159453, -1281479697, -340305902))
      r
    }

    test("sequence") {
      test("the same as ints") {
        val (r, _) = sequence(List(int, int, int)).run(SimpleRNG(42L))
        assert(r == List(16159453, -1281479697, -340305902))
        r
      }
    }

    test("map") {
      test("converts int gen to 'int dividable by 2' gen") {
        multipleAsserts(map(int)(_ * 2).run, (i: Int) => i % 2 == 0, 1000)
      }

      test("reproducibility") {
        val r = map(int)(_ * 2).run(SimpleRNG(42L))._1
        assert(r == 32318906)
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
