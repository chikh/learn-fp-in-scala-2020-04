package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = State { rng => (a, rng) }

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = State { rng =>
    val (a, next) = ra.run(rng)
    f(a).run(next)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(unit[B] _ compose f)
  /*
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, nextRng) = s(rng)
    (f(a), nextRng)
  }
   */

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  /*
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 => {
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
   */

  def both[A, B](ra: Rand[A], rb: Rand[B]) = map2(ra, rb)((_, _))

  def sequence[A](l: List[Rand[A]]): Rand[List[A]] = l match {
    case Nil    => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  /*@tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = int(rng)
    if (i == Int.MinValue) nonNegativeInt(nextRng)
    else (Math.abs(i), nextRng)
  }*/

  def nonNegativeInt: Rand[Int] =
    flatMap(int)(i =>
      if (i == Int.MinValue) nonNegativeInt
      else unit(Math.abs(i))
    )

  def int: Rand[Int] = State { _.nextInt }

  def double: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble)
  /*def double(rng: RNG): (Double, RNG) = {
    val (positiveInt, nextRng) = nonNegativeInt(rng)
    (positiveInt / (Int.MaxValue: Double), nextRng)
  }*/

  def intDouble: Rand[(Int, Double)] = both(int, double)
  /*def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, nextNextRng) = double(nextRng)
    ((i, d), nextNextRng)
  }*/

  def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))
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
