package testing

import utest._
import state._
import Prop._

object PropTests extends TestSuite {

  def tests = Tests {
    test("&& combinator") {
      test("successful Prop combined twice") {
        val p = Prop((_, _) => Success)
        val r = p && p

        assert(r.run(42, SimpleRNG(42L)) == Success)
        r
      }
    }

    test("forAll") {
      def intSeqRNG(counter: Int): RNG = new RNG {
        override def nextInt: (Int, RNG) =
          (counter, intSeqRNG(counter + 1))
      }

      val r = forAll(Gen(RNG.int))(_ % 3 != 0).run(42, intSeqRNG(1))

      assert(r == Failure("3", 2))
      r
    }
  }
}
