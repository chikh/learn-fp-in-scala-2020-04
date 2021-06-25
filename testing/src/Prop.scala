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

case class Gen[+A](state: State[RNG, A]) {
  def map[B](f: A => B) = Gen.map(f)(this)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(f)(this)
}

object Gen {
  def map[A, B](f: A => B): Gen[A] => Gen[B] = genA => Gen(genA.state.map(f))

  def flatMap[A, B](f: A => Gen[B]): Gen[A] => Gen[B] =
    genA => Gen(genA.state.flatMap(a => f(a).state))

  def listOfN[A](n: Gen[Int]): Gen[A] => Gen[List[A]] =
    genA => n.flatMap(listOfN(_, genA))

  def choose(min: Int, maxExclusive: Int): Gen[Int] =
    Gen(RNG.intInInterval(min, maxExclusive))

  def intPair(min: Int, max: Int): Gen[(Int, Int)] =
    Gen(State.both(RNG.intInInterval(min, max), RNG.intInInterval(min, max)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(RNG.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.state)))

  def genOption[A]: Gen[A] => Gen[Option[A]] = map(Option(_))

  def genAfromOptionV1[A]: Gen[Option[A]] => Gen[A] =
    genA => Gen(genA.state.map(_.getOrElse(???)))

  def genAfromOptionV2[A]: Gen[Option[A]] => Gen[A] =
    map(_.getOrElse(???))

  def char: Gen[Char] = choose(21, 126).map(_.toChar)

  def string(length: Int): Gen[String] = listOfN(length, char).map(_.mkString)
}
