package testing

import state._
import scala.util.control.NonFatal
import Prop._

sealed trait Result

case class Failure(failedItem: FailedItem, successCount: SuccessCount)
    extends Result
case object Success extends Result

case class Prop(run: (NumberOfRuns, RNG) => Result) {
  def &&(other: Prop): Prop =
    Prop { (numberOfRuns, rng) =>
      this.run(numberOfRuns, rng) match {
        case Success =>
          other.run(numberOfRuns, rng) match {
            case f: Failure =>
              f.copy(successCount = numberOfRuns + f.successCount)
            case s => s
          }

        case f: Failure => f
      }
    }

  def ||(other: Prop): Prop = Prop { (n, rng) =>
    this.run(n, rng) match {
      case s @ Success => s
      case Failure(firstFailureMessage, c) =>
        other.tag(firstFailureMessage).run(n, rng) match {
          case f: Failure  => f.copy(successCount = c + f.successCount)
          case s @ Success => s
        }
    }
  }

  def tag(label: String) = Prop {
    (n, rng) => this.run(n, rng) match {
      case s @ Success => s
      case f @ Failure(message, _) => f.copy(failedItem = s"$label\n$message")
    }
  }
}

object Prop {
  type FailedItem = String
  type SuccessCount = Int
  type NumberOfRuns = Int

  /**
    * This impl pulls (generates) all the values before executing condition function
    */
  def forAllUnlazy[A](g: Gen[A])(condition: A => Boolean): Prop = Prop {
    (numberOfRuns, initialRNG) =>
      Gen
        .listOfN(numberOfRuns, g)
        .map(_.zipWithIndex.foldLeft(Success: Result) {
          case (Success, (a, index)) =>
            if (condition(a)) Success
            else Failure(a.toString, index)

          case (f: Failure, _) => f
        })
        .state
        .run(initialRNG)
        ._1
  }

  def forAll[A](g: Gen[A])(condition: A => Boolean): Prop = Prop {
    (numberOfRuns, initialRNG) =>
      randomStream(initialRNG)(g)
        .zip(LazyList.from(0))
        .take(numberOfRuns)
        .map {
          case (a, index) =>
            try {
              if (condition(a)) Success
              else Failure(a.toString, index)
            } catch {
              case NonFatal(e) =>
                Failure(
                  s"test case: $a\nthrowed: ${e.getMessage}\nstacktrace:\n${e.getStackTrace
                    .mkString("\n")}",
                  index
                )
            }
        }
        .find {
          case _: Failure => true
          case _          => false
        }
        .getOrElse(Success)
  }

  def randomStream[A](initialRNG: RNG)(g: Gen[A]): LazyList[A] =
    LazyList.unfold(initialRNG)(rng => Some(g.state.run(rng)))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen {
    i => this.forSize(i).map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    i => this.forSize(i).flatMap(a => f(a).forSize(i))
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))
}

case class Gen[+A](state: State[RNG, A]) {
  def map[B](f: A => B) = Gen.map(f)(this)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(f)(this)

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def map[A, B](f: A => B): Gen[A] => Gen[B] = genA => Gen(genA.state.map(f))

  def flatMap[A, B](f: A => Gen[B]): Gen[A] => Gen[B] =
    genA => Gen(genA.state.flatMap(a => f(a).state))

  def listOfN[A](n: Gen[Int]): Gen[A] => Gen[List[A]] =
    genA => n.flatMap(listOfN(_, genA))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val totalWeight = g1._2.abs + g2._2.abs
    val w1 = g1._2 / totalWeight

    double.flatMap(d => if (d <= w1) g1._1 else g2._1)
  }

  def double: Gen[Double] = Gen(RNG.double)

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
