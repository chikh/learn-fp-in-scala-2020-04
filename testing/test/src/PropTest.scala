package testing

import utest._

object PropTests extends TestSuite {

  def tests = Tests {
    test("&& combinator") {
      test("successful Prop combined twice") {
        val p = new Prop {
          def check = Right(42)
        }

        val r = p && p

        assert(r.check == Right(84))
        r
      }
    }
  }
}
