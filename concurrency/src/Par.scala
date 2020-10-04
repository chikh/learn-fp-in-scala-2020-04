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

  def map[A, B](f: A => B): Par[A] => Par[B] = parA => map2(parA, unit(()))((a, _) => f(a))

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

class CompletedFuture[A](val get: A) extends Future[A] {

  override def cancel(x: Boolean): Boolean = false

  override def isCancelled(): Boolean = false

  override def isDone(): Boolean = true

  override def get(x$1: Long, x$2: TimeUnit): A = get
}