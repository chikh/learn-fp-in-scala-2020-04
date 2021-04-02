package concurrency

import utest._
import Par._
import java.util.concurrent.Executors

object ParTests extends TestSuite {
  val fixedPool = Executors.newFixedThreadPool(Int.MaxValue)

  def tests = Tests {
    test("parallel sum") {
      test("the result is correct") {
        val r = run(sum(Vector(1, 2, 3, 4, 5)))(fixedPool)
        assert(r == 15)
        r
      }
    }

    test("sequence") {
      val r = run(sequence(List(lazyUnit(42),  lazyUnit(44))))(fixedPool)
      assert(r == List(42, 44))
      r
    }

    test("parMap") {
      val l = (1 to 1024).toList
      val f: Int => String = _.toString
      val r = run(parMap(l)(f))(fixedPool)
      assert(r == l.map(f))
    }

    test("filter") {
      val l = (1 to 1024).toList
      val p: Int => Boolean = _ % 2 == 0
      val r = run(parFilter(l)(p))(fixedPool)
      assert(r == l.filter(p))
    }
  }
}
