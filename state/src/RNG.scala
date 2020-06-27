package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = State[RNG, A]

  /*@tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = int(rng)
    if (i == Int.MinValue) nonNegativeInt(nextRng)
    else (Math.abs(i), nextRng)
  }*/
  def nonNegativeInt: Rand[Int] =
    int.flatMap(i =>
      if (i == Int.MinValue) nonNegativeInt
      else State.unit(Math.abs(i))
    )

  def int: Rand[Int] = State { _.nextInt }

  def double: Rand[Double] = nonNegativeInt.map(_ / Int.MaxValue.toDouble)
  /*def double(rng: RNG): (Double, RNG) = {
    val (positiveInt, nextRng) = nonNegativeInt(rng)
    (positiveInt / (Int.MaxValue: Double), nextRng)
  }*/

  def intDouble: Rand[(Int, Double)] = State.both(int, double)
  /*def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, nextNextRng) = double(nextRng)
    ((i, d), nextNextRng)
  }*/

  def ints(n: Int): Rand[List[Int]] = State.sequence(List.fill(n)(int))
  /*def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    if (n > 0) {
      val (i, nextRng) = rng.nextInt
      val (nextInts, nextNextRng) = ints(n - 1)(nextRng)
      (i :: nextInts, nextNextRng)
    } else (Nil, rng)
  }*/
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
