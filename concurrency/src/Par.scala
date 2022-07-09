package concurrency

trait Par[A]

object Par {
  def unit[A](a: => A): Par[A] = ???
  def get[A](p: Par[A]): A = ???
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???

  def parSum(l: IndexedSeq[Int]): Int = ???
}
