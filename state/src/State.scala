package state

import State._

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { rng =>
    val (a, next) = run(rng)
    f(a).run(next)
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(unit[S, B] _ compose f)
  /*
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, nextRng) = s(rng)
    (f(a), nextRng)
  }
   */

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))
  /*
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 => {
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
 */
}

object State {
  def unit[S, A](a: A): State[S, A] = State { rng => (a, rng) }

  def both[S, A, B](ra: State[S, A], rb: State[S, B]) = ra.map2(rb)((_, _))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](Nil))((ra, acc) => ra.map2(acc)(_ :: _))
}
