package rng

import scala.annotation.tailrec

trait RNG[A] {
  def next: (A, RNG[A])
}

object RNG {
  @tailrec
  def nonNegativeInt(rng: RNG[Int]): (Int, RNG[Int]) = {
    val (i, nextRng) = rng.next
    if (i < 0) nonNegativeInt(nextRng)
    else (i, nextRng)
  }
}

case class SimpleRNG(seed: Long) extends RNG[Int] {
  override def next: (Int, RNG[Int]) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
