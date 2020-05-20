package nonstrictness

import dataStructures.{List, Nil => ListNil, Cons => ListCons}
import dataStructures.{Option, None, Some}

sealed trait Stream[+A] {
  def toList = Stream.toList(this)
  def take = Stream.take(this) _
  def foldRight[B] = Stream.foldRight[A, B](this) _
  def forAll = Stream.forAll(this) _
  def takeWhile = Stream.takeWhile(this) _
  def headOption = Stream.headOption(this)
  def map[B] = Stream.map[A, B](this) _
  def filter = Stream.filter(this) _
  def append[B >: A] = Stream.append[B](this) _
  def flatMap[B] = Stream.flatMap[A, B](this) _
  def zipWith[B >: A] = Stream.zipWith[B](this) _
  def zipAll[B] = Stream.zipAll[A, B](this) _
  def startsWith[B >: A] = Stream.startsWith[B](this) _
  def tails = Stream.tails(this)
  def exists = Stream.exists(this) _
  def hasSubsequence[B >: A] = Stream.hasSubsequence[B](this) _
  def scanRight[B] = Stream.scanRight[A, B](this) _
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

  def unfold[A, S](z: => S)(f: S => Option[(() => A, () => S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a(), unfold(s())(f))
      case None         => nil[A]
    }

  def zipWith[A](s1: Stream[A])(s2: Stream[A])(f: (A, A) => A): Stream[A] =
    unfold((s1, s2)) {
      case (Nil, _) => None
      case (_, Nil) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((() => f(h1(), h2()), () => (t1(), t2())))
    }

  def zipAll[A, B](
      sa: Stream[A]
  )(sb: Stream[B]): Stream[(Option[A], Option[B])] = unfold((sa, sb)) {
    case (Nil, Cons(h, t)) =>
      Some((() => (None, Some(h())), () => (nil[A], t())))
    case (Cons(h, t), Nil) =>
      Some((() => (Some(h()), None), () => (t(), nil[B])))
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Some((() => (Some(h1()), Some(h2())), () => (t1(), t2())))
    case _ => None
  }

  def startsWith[A](s: Stream[A])(prefix: Stream[A]): Boolean =
    zipAll(s)(prefix)
      .takeWhile {
        case (_, Some(_)) => true
        case _            => false
      }
      .forAll {
        case (a1, a2) => a1 == a2
      }

  /**
    * Another implementation option was: cons(s, unfold(...using only tails, not cons itself...)), but it uses quite low-level `cons` which not that elegant
    */
  def tails[A](s: Stream[A]): Stream[Stream[A]] =
    unfold(s) {
      case c @ Cons(_, t) => Some((() => c, () => t()))
      case Nil            => None
    } append Stream(nil)

  def exists[A](s: Stream[A])(p: A => Boolean): Boolean =
    foldRight(s)(false)((a, acc) => p(a) || acc)

  def hasSubsequence[A](s: Stream[A])(sub: Stream[A]): Boolean =
    s.tails exists (_ startsWith sub)

  // TODO: test it maybe
  def scanLeft[A, B](s: Stream[A])(z: => B)(f: (=> A, => B) => B): Stream[B] =
    cons(z, nil) append unfold((s, z)) {
      case (Nil, _) => None
      case (Cons(h, t), b) =>
        lazy val nextB = f(h(), b)
        Some((() => nextB, () => (t(), nextB)))
    }

  def scanRight[A, B](s: Stream[A])(z: => B)(f: (=> A, => B) => B): Stream[B] =
    foldRight(s)((z, Stream(z))) {
      case (a, (b, sb)) =>
        lazy val nextB = f(a, b)
        (nextB, cons(nextB, sb))
    }._2
}
