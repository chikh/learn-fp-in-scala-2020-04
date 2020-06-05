package state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, nextRng) = s(rng)
    (f(a), nextRng)
  }

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(nextRng)
    else (Math.abs(i), nextRng)
  }

  def double: Rand[Double]= map(nonNegativeInt)(_ / Int.MaxValue.toDouble)
  /*def double(rng: RNG): (Double, RNG) = {
    val (positiveInt, nextRng) = nonNegativeInt(rng)
    (positiveInt / (Int.MaxValue: Double), nextRng)
  }*/

  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    if (n > 0) {
      val (i, nextRng) = rng.nextInt
      val (nextInts, nextNextRng) = ints(n - 1)(nextRng)
      (i :: nextInts, nextNextRng)
    } else (Nil, rng)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
