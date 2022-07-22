package parallel

import utest._
import Par._
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object ParTests extends TestSuite {
  implicit val es: ExecutorService = Executors.newFixedThreadPool(3)

  def tests = Tests {
    test("sum in parallel") {
      val s = Vector(1, 2, 3, 4, 5)

      val r = run(parSum(s))
      test("result is the same as normal sum") {
        assert(r == s.sum)
        r
      }
    }
    
    test("map2 is lazy") {
      @volatile
      var cnt = 0

      val p = Par(() => { cnt += 1; cnt }, false)

      map2(p, p)(_ + _)

      assert(cnt == 0)
      cnt
    }
  }
}
