package testing

import utest._
import state._
import Prop._

object PropTests extends TestSuite {

  def tests = Tests {
    def someFailureItem(successCount: Int) = Failure("some item", successCount)

    val successP = Prop((_, _) => Success)
    def failureP(successCount: Int) =
      Prop((_, _) => someFailureItem(successCount))

    test("&& combinator") {
      test("successful Prop combined twice") {
        val r = successP && successP

        assert(r.run(42, SimpleRNG(42L)) == Success)
        r
      }

      test("one failure") {
        val generatedItemCausingFailure = List(42)
        val testsAmountToRun = 100
        val empiricalSuccessfulListsAmountBeforeFailure = 4

        def bogusReverseList[A](l: List[A]): List[A] =
          if (l == generatedItemCausingFailure) List()
          else l.reverse

        val p = forAll(Gen.listOfN(5, Gen.choose(40, 50)))(l =>
          bogusReverseList(bogusReverseList(l)) == l
        ) && forAll(Gen.listOfN(1, Gen.choose(40, 50)))(l =>
          bogusReverseList(bogusReverseList(l)) == l
        )

        val r = p.run(testsAmountToRun, SimpleRNG(1L))
        assert(
          r == Failure(
            generatedItemCausingFailure.toString,
            testsAmountToRun + empiricalSuccessfulListsAmountBeforeFailure
          )
        )
        r
      }
    }

    test("|| combinator") {
      test("both successful") {
        val r = (successP || successP).run(42, SimpleRNG(42L))

        assert(r == Success)
        r
      }

      test("success || failure") {
        val r = (successP || failureP(42)).run(42, SimpleRNG(42L))

        assert(r == Success)
        r
      }

      test("failure || success") {
        val r = (failureP(42) || successP).run(42, SimpleRNG(42L))

        assert(r == Success)
        r
      }

      test("failure || failure") {
        val successCount1 = 3
        val successCount2 = 4
        val r = (failureP(successCount1) || failureP(successCount2))
          .run(42, SimpleRNG(42L))
        val expectedFailureItem = someFailureItem(successCount1 + successCount2)

        assert(
          r == expectedFailureItem.copy(failedItem =
            List.fill(2)(expectedFailureItem.failedItem).mkString("\n")
          )
        )
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
