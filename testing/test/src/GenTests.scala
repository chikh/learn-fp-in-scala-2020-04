package testing

import utest._
import state._

object GenTests extends TestSuite {

  def tests = Tests {
    test("Gen") {
      import Gen._

      test("choose int generator") {
        test("should generate values in range [1, 4), so 1, 2 or 3") {
          val min = 1
          val maxExcl = 4

          val l = manyGeneratorStates(choose(min, maxExcl))

          assert(l.run(SimpleRNG(42L))._1.forall(i => i >= min && i < maxExcl))
        }
      }

      test("unit") {
        val a = 42
        val g = unit(a)
        assert(g.state.run(SimpleRNG(42L))._1 == a)
      }

      test("boolean") {
        val l = manyGeneratorStates(boolean).run(SimpleRNG(42L))._1
        val epsilonPart = 0.1

        test("approx. half should be true") {
          assert(
            l.count(_ == true) > generatorTriggeredTimes * (0.5 - epsilonPart)
          )
          assert(
            l.count(_ == true) < generatorTriggeredTimes * (0.5 + epsilonPart)
          )
        }

        test("approx. half should be false") {
          assert(
            l.filter(_ == false)
              .size > generatorTriggeredTimes * (0.5 - epsilonPart)
          )
          assert(
            l.filter(_ == false)
              .size < generatorTriggeredTimes * (0.5 + epsilonPart)
          )
        }
      }

      test("listOfN") {
        val min = 1
        val maxExcl = 4

        val l = listOfN(generatorTriggeredTimes, choose(min, maxExcl)).state

        test("should generate values in range [1, 4), so 1, 2 or 3") {
          assert(l.run(SimpleRNG(42L))._1.forall(i => i >= min && i < maxExcl))
        }
      }

      test("intPair") {
        val min = 1
        val maxExcl = 4

        val l = manyGeneratorStates(intPair(min, maxExcl))

        val r = l.run(SimpleRNG(42L))._1

        test("should generate values in range [1, 4), so 1, 2 or 3") {
          assert(r.forall {
            case (i1, i2) =>
              i1 >= min && i1 < maxExcl && i2 >= min && i2 < maxExcl
          })
        }

        test("Numbers should be sometimes different in the pair") {
          assert(r.exists { case (i1, i2) => i1 != i2 })
        }
      }

      test("Gen[Option[A]] from Gen[A]") {
        val a = boolean
        val r = genOption(a)
        val seed = SimpleRNG(42L)

        assert(Some(a.state.run(seed)._1) == r.state.run(seed)._1)
        r.state.run(seed)
      }

      test("string") {
        val l = 42
        val r = string(l).state.run(SimpleRNG(40L))._1
        assert(r.length == l)
        r
      }
    }

    test("SGen") {
      import SGen._
      import Gen._

      test("listOf") {
        val min = 1
        val maxExcl = 4

        val l =
          listOf(choose(min, maxExcl)).forSize(generatorTriggeredTimes).state

        test("should generate values in range [1, 4), so 1, 2 or 3") {
          assert(l.run(SimpleRNG(42L))._1.forall(i => i >= min && i < maxExcl))
        }
      }
    }
  }

  val generatorTriggeredTimes: Int = 2100

  def manyGeneratorStates[A](g: Gen[A]) =
    State.sequence(List.fill(generatorTriggeredTimes)(g).map(_.state))

}
