package testing

import state._

trait Prop {
  type SuccessCount = Int
  type FailedItem = String

  def check: Either[(FailedItem, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop =
    new Prop {
      def check = Prop.this.check.flatMap(count => p.check.map(_ + count))
    }
}

case class Gen[+A](state: State[RNG, A])

object Gen {
  def choose(min: Int, maxExclusive: Int): Gen[Int] = {
    def intInInterval(min: Int, maxExclusive: Int): State[RNG, Int] =
      RNG.nonNegativeInt.map(_ % (maxExclusive - min)).map(_ + min)

    Gen(intInInterval(min, maxExclusive))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(RNG.boolean)
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.state)))
}
