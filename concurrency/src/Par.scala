package concurrency

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](par: Par[A])(es: ExecutorService): A = par(es).get

  def fork[A](par: => Par[A]): Par[A] = es => es.submit(() => par(es).get)

  def unit[A](a: A): Par[A] = _ => new CompletedFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = es => {
    val a = parA(es)
    val b = parB(es)

    new CompletedFuture(f(a.get, b.get))
  }

  def map[A, B](f: A => B): Par[A] => Par[B] =
    parA => map2(parA, unit(()))((a, _) => f(a))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List.empty[A]))(
      (parA, parList) => map2(parA, parList)(_ :: _)
    )

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parFoldRight[A, B](seq: IndexedSeq[A])(z: B)(f: (A, B) => B)(combinator: (B, B) => B): Par[B] =
    if (seq.length <= 1) {
      unit(seq.headOption.map(h => f(h, z)).getOrElse(z))
    } else {
      val (l, r) = seq.splitAt(seq.length / 2)

      map2(
        fork(parFoldRight(l)(z)(f)(combinator)),
        fork(parFoldRight(r)(z)(f)(combinator))
      )(combinator)
    }

  def sum(seq: IndexedSeq[Int]): Par[Int] = parFoldRight(seq)(0)(_ + _)(_ + _)

  def parMax(seq: IndexedSeq[Int]): Par[Option[Int]] =
    parFoldRight(seq)(None: Option[Int]) {
      case (a, Some(b)) => Some(a max b)
      case (a, None) => Some(a)
    } {
      case (Some(a), Some(b)) => Some(a max b)
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (None, None) => None
    }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(as.map(asyncF(f)))
  }

  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = /*
  // The "straighforward" impl
  fork {
    val parP = asyncF(p)
    as.foldRight(unit(List.empty[A]))((a, parListA) => map2(parP(a), parListA)((isKeep: Boolean, listA) => if (isKeep) a :: listA else listA))
  }*/

  // Let's reuse as.foldRight from #sequence
  {
    val parP: A => Par[List[A]] = asyncF { a =>
      if (p(a)) List(a) else Nil
    }

    map[List[List[A]], List[A]](_.flatten)(sequence(as.map(parP)))

  }
}

class CompletedFuture[A](val get: A) extends Future[A] {

  override def cancel(x: Boolean): Boolean = false

  override def isCancelled(): Boolean = false

  override def isDone(): Boolean = true

  override def get(x$1: Long, x$2: TimeUnit): A = get
}
