package parallel

import utest._
import Par._
import java.util.concurrent.Executors

object ParTests extends TestSuite {
  def tests = Tests {
    test("ex usage: sum in parallel") {
      val s = Vector(1, 2, 3, 4, 5)

      val r = ExampleParUsage
        .parSum(s)
        .run(Executors.newFixedThreadPool(3))
        .get

      test("result is the same as normal sum") {
        assert(r == s.sum)
        r
      }
    }

    test("map2 is lazy") {
      @volatile
      var cnt = 0

      val p: Par[Int] = _ => { cnt += 1; UnitFuture(cnt) }

      p.map2(p)(_ + _)

      assert(cnt == 0)
      cnt
    }

    test("sequence") {
      val r = sequence(List(unit(1), unit(2), unit(3)))
        .run(Executors.newFixedThreadPool(1))
        .get

      assert(r == List(1, 2, 3))
    }

    test("parFilter") {
      val r = parFilter(List(1, 2, 3))(_ != 2)
        .run(Executors.newFixedThreadPool(1))
        .get

      assert(r == List(1, 3))
    }
  }
}
