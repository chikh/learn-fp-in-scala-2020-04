package nonstrictness

import dataStructures.{List, Nil => ListNil, Cons => ListCons}
import dataStructures.{Option, None, Some}

sealed trait Stream[+A] {
  def toList = Stream.toList(this)
  def take = Stream.take(this) _
  def foldRight[B] = (z: B) => Stream.foldRight(this)(z) _
  def forAll = Stream.forAll(this) _
  def takeWhile = Stream.takeWhile(this) _
  def headOption = Stream.headOption(this)
  def map[B] = Stream.map[A, B](this) _
  def filter = Stream.filter(this) _
  def append[B >: A] = Stream.append[B](this) _
  def flatMap[B] = Stream.flatMap[A, B](this) _
}

final case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Nil extends Stream[Nothing]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def nil[A]: Stream[A] = Nil

  def apply[A](xs: A*): Stream[A] =
    if (xs.isEmpty) nil else cons(xs.head, apply(xs.tail: _*))

  def toList[A](s: Stream[A]): List[A] = s match {
    case Nil        => ListNil
    case Cons(h, t) => ListCons(h(), toList(t()))
  }

  def take[A](s: Stream[A])(n: Int): Stream[A] = s match {
    case Cons(a, t) if n > 0 => Cons(a, () => take(t())(n - 1))
    case _                   => Nil
  }

  def foldRight[A, B](s: Stream[A])(z: => B)(f: (=> A, => B) => B): B =
    s match {
      case Nil        => z
      case Cons(h, t) => f(h(), foldRight(t())(z)(f))
    }

  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(s)(nil[A])((a, as) => if (p(a)) cons(a, as) else nil)

  def forAll[A](s: Stream[A])(p: A => Boolean): Boolean =
    foldRight(s)(true)((a, b) => p(a) && b)

  def headOption[A](s: Stream[A]): Option[A] =
    foldRight(s)(None: Option[A])((a, _) => Some(a))

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] =
    foldRight(s)(nil[B])((a, b) => cons(f(a), b))

  /**
    * Think of a function which cuts the beginning of the s: Stream up until the first matching element, which will be the first element of the result stream. It's needed also to put nil (empty) or cons as an implementation of the result stream. So it can't be completely lazy with this implementation.
    */
  def filter[A](s: => Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(s)(nil[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[A](s1: => Stream[A])(s2: => Stream[A]): Stream[A] =
    foldRight(s1)(s2)((a, s) => cons(a, s))

  def flatMap[A, B](s: Stream[A])(f: A => Stream[B]): Stream[B] =
    foldRight(s)(nil[B])((a, b) => append(f(a))(b))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(current: Int, next: Int): Stream[Int] =
      cons(current, go(next, current + next))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => nil[A]
    }
}
