package concurrency

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

sealed trait Par[A]

case class InThisThread[A](a: A) extends Par[A]
case class InNewThread[A](a: () => A) extends Par[A]

object Par {
  def run[A](par: Par[A]): A = par match {
    case InThisThread(a) => a
    case InNewThread(a) => Await.result(Future(a())(scala.concurrent.ExecutionContext.global), Duration.Inf)
  }

  def fork[A](par: => Par[A]): Par[A] = InNewThread(() => run(par))

  def unit[A](a: A): Par[A] = InThisThread(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = (parA, parB) match {
    case (InThisThread(a), InThisThread(b)) => InThisThread(f(a, b))
    case (InNewThread(a), InNewThread(b)) => InNewThread(() => f(a(), b()))
    case (InThisThread(a), InNewThread(b)) => InNewThread(() => f(a, b()))
    case (InNewThread(a), InThisThread(b)) => InNewThread(() => f(a(), b))
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List.empty[A]))((parA, parList) => map2(parA, parList)(_ :: _))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sum(seq: IndexedSeq[Int]): Par[Int] =
    if (seq.length <= 1) {
      unit(seq.headOption getOrElse 0)
    } else {
      val (l, r) = seq.splitAt(seq.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }
}
