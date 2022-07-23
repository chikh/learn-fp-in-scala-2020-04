package parallel

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](pa: Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        override def call: A = pa(es).get // TODO: don't waste threads
      })

  def run[A](es: ExecutorService)(p: Par[A]): Future[A] = p(es)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      UnitFuture(
        f(pa.run(es).get, pb.run(es).get) // TODO: get rid of all the blocking
      )
    }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  implicit class ParOps[A](pa: Par[A]) {
    def run(es: ExecutorService): Future[A] = Par.run(es)(pa)
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = Par.map2(pa, pb)(f)
  }

}

object ExampleParUsage {
  import Par._

  def parSum(s: IndexedSeq[Int]): Par[Int] = {
    if (s.length <= 1) unit(s.headOption.getOrElse(0))
    else {
      val (l, r) = s.splitAt(s.length / 2)
      fork(parSum(l)).map2(fork(parSum(r)))(_ + _)
    }
  }
}

case class UnitFuture[A](a: A) extends Future[A] {
  def cancel(x: Boolean): Boolean = false

  def isCancelled(): Boolean = false

  def isDone(): Boolean = true

  def get(): A = a

  def get(x: Long, y: TimeUnit): A = a

}
