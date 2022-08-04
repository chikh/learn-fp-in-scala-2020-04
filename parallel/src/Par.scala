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

  def map[A, B](f: A => B): Par[A] => Par[B] =
    _.map2(unit(()))((a, _) => f(a))

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(
      f: (A, B, C) => D
  ): Par[D] =
    map2(map2(pa, pb)((a, b) => (a, b)), pc) { case ((a, b), c) => f(a, b, c) }

  def map4[T1, T2, T3, T4, R](
      p1: Par[T1],
      p2: Par[T2],
      p3: Par[T3],
      p4: Par[T4]
  )(
      f: (T1, T2, T3, T4) => R
  ): Par[R] =
    map2(map2(p1, p2)((t1, t2) => (t1, t2)), map2(p3, p4)((t3, t4) => (t3, t4))) {
      case ((t1, t2), (t3, t4)) => f(t1, t2, t3, t4)
    }

  def map5[T1, T2, T3, T4, T5, R](
      p1: Par[T1],
      p2: Par[T2],
      p3: Par[T3],
      p4: Par[T4],
      p5: Par[T5]
  )(
      f: (T1, T2, T3, T4, T5) => R
  ): Par[R] =
    map2(
      map2(p1, p2)((t1, t2) => (t1, t2)),
      map2(p3, map2(p4, p5)((t4, t5) => (t4, t5))) {
        case (t3, (t4, t5)) => (t3, t4, t5)
      }
    ) {
      case ((t1, t2), (t3, t4, t5)) => f(t1, t2, t3, t4, t5)
    }

  def sequence[A](lp: List[Par[A]]): Par[List[A]] =
    lp.foldRight(unit(List.empty[A]))((pa, pl) => pa.map2(pl)(_ :: _))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f))).fork

  def parFilter_highlevel[A](la: List[A])(p: A => Boolean): Par[List[A]] = {
    sequence(la.map(asyncF(a => if (p(a)) List(a) else List.empty)))
      .map(_.flatten)
  }

  def parFilter[A](la: List[A])(p: A => Boolean): Par[List[A]] =
    la.foldRight(unit(List.empty[A]))((a, pl) =>
      if (p(a)) pl.map(a :: _) else pl
    )

  def parFold[A](zero: A)(f: (A, A) => A)(s: IndexedSeq[A]): Par[A] = {
    def nextCall: IndexedSeq[A] => Par[A] = parFold(zero)(f)

    if (s.length <= 1) unit(s.headOption.getOrElse(zero))
    else {
      val (l, r) = s.splitAt(s.length / 2)
      fork(nextCall(l)).map2(fork(nextCall(r)))(f(_, _))
    }
  }

  implicit class ParOps[A](pa: Par[A]) {
    def run(es: ExecutorService): Future[A] = Par.run(es)(pa)
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = Par.map2(pa, pb)(f)
    def fork = Par.fork(pa)
    def map[B](f: A => B) = Par.map(f)(pa)
  }

}

object ExampleParUsage {
  import Par._

  def parSum(s: IndexedSeq[Int]): Par[Int] =
    parFold(0)(_ + _)(s)

  def parMax(s: IndexedSeq[Int]): Par[Option[Int]] =
    parFold(None: Option[Int])((lo, ro) =>
      for {
        l <- lo
        r <- ro
      } yield l max r
    )(s.map(Some(_)))
}

case class UnitFuture[A](a: A) extends Future[A] {
  def cancel(x: Boolean): Boolean = false

  def isCancelled(): Boolean = false

  def isDone(): Boolean = true

  def get(): A = a

  def get(x: Long, y: TimeUnit): A = a

}
