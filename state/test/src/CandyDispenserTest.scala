package state.examples

import utest._
import Machine._

object CandyDispenserTests extends TestSuite {
  override def tests: Tests = Tests {
    test("the simulation") {
      test("insert should unlock the locked with candies") {
        val r = simulate(List(Insert))
          .run(Machine(Locked(true), Candies(42), Coins(42)))
          ._2
        assert(r == Machine(Locked(false), Candies(42), Coins(42)))
        r
      }

      test("insert should do nothing to the unlocked") {
        val r = simulate(List(Insert))
          .run(Machine(Locked(false), Candies(42), Coins(42)))
          ._2
        assert(r == Machine(Locked(false), Candies(42), Coins(42)))
        r
      }

      test("insert should do nothing if there's no candies") {
        val r = simulate(List(Insert))
          .run(Machine(Locked(true), Candies(0), Coins(42)))
          ._2
        assert(r == Machine(Locked(true), Candies(0), Coins(42)))
        r
      }

      test("turn should do nothing if locked") {
        val r = simulate(List(Turn))
          .run(Machine(Locked(true), Candies(42), Coins(42)))
          ._2
        assert(r == Machine(Locked(true), Candies(42), Coins(42)))
        r
      }

      test("turn should consume coins, dispence candy and lock if unlocked") {
        val r = simulate(List(Turn))
          .run(Machine(Locked(false), Candies(42), Coins(42)))
          ._2
        assert(r == Machine(Locked(true), Candies(41), Coins(43)))
        r
      }

      test(
        "if the machine has 10 coins and 5 candies and 4 candies bought, the simulation should return 14 coins and 1 candy"
      ) {
        val r = simulate(
          List(Insert, Turn, Insert, Turn, Insert, Turn, Insert, Turn)
        ).run(Machine(Locked(false), Candies(5), Coins(10)))._1
        assert(r == ((Candies(1) -> Coins(14))))
        r
      }
    }
  }
}
