package parallel

import java.util.concurrent.Executors
import java.util.concurrent.Callable

case class Par[A](a: () => A, forked: Boolean)

object Par {
  def unit[A](a: => A): Par[A] = Par(() => a, false)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](pa: Par[A]): Par[A] = pa match {
    case Par(a, false) => Par(a, true)
    case p => p
  }

  def run[A](p: Par[A]): A = p match {
    case Par(a, false) => a()
    case Par(a, true) => Executors.newFixedThreadPool(1).submit(new Callable[A] {
      override def call: A = a()
    }).get()
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    unit(f(run(pa), run(pb)))

  def parSum(s: IndexedSeq[Int]): Par[Int] = {
    if (s.length <= 1) unit(s.headOption.getOrElse(0))
    else {
      val (l, r) = s.splitAt(s.length / 2)
      map2(fork(parSum(l)), fork(parSum(r)))(_ + _)
    }
  }
}
