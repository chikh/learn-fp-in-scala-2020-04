package state

import utest._
import State._

object StateTests extends TestSuite {
  override def tests: Tests = Tests {
    test("get") {
      val state = SimpleRNG(42L)
      val r = get.run(state)
      assert(r._1 == state)
      r
    }

    test("set") {
      val newState = SimpleRNG(44L)
      val oldState = SimpleRNG(42L)
      val r = set(newState).run(oldState)
      assert(r == () -> newState)
      r
    }

    test("modify") {
      val oldState = SimpleRNG(42L)
      val f: SimpleRNG => SimpleRNG = rng => SimpleRNG(rng.seed + 1)
      val r = modify(f).run(oldState)
      assert(r == () -> SimpleRNG(43L))
      r
    }
  }
}
