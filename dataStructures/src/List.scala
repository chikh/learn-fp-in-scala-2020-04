package dataStructures

import scala.annotation.tailrec

sealed trait List[+A]

final case class Cons[A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))

  def length(as: List[_]) =
    foldLeft(as)(0)((b, _) => b + 1)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
    else
      l match {
        case Nil        => throw new NoSuchElementException("can't drop")
        case Cons(_, t) => drop(t, n - 1)
      }

  def foldRight[A, B](l: List[A])(z: B)(op: (A, B) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => op(h, foldRight(t)(z)(op))
  }

  @tailrec
  def foldLeft[A, B](l: List[A])(z: B)(op: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t)(op(z, h))(op)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l)(Nil: List[A])((b, a) => Cons(a, b))

  def foldRightViaLeft[A, B](l: List[A])(z: B)(op: (A, B) => B): B =
    (((r: List[A]) => foldLeft(r)(z)((b, a) => op(a, b))) compose reverse[A])(l)

  def append[A](l1: List[A])(l2: List[A]): List[A] =
    foldRightViaLeft(l1)(l2)(Cons(_, _))

  def concat[A](ls: List[List[A]]): List[A] =
    foldRightViaLeft(ls)(Nil: List[A])(append(_)(_))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightViaLeft(as)(Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)
    // foldRightViaLeft(as)(Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRightViaLeft(as)(Nil: List[B])((a, b) => append(f(a))(b))

  def zipWith[A](as1: List[A])(as2: List[A])(f: (A, A) => A): List[A] =
    as1 match {
      case Nil => Nil
      case Cons(h1, t1) => as2 match {
        case Nil => Nil
        case Cons(h2, t2) => Cons(f(h1, h2), zipWith(t1)(t2)(f))
      }
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case l if (l == sub) => true
    case Nil => false
    case Cons(_, t) => hasSubsequence(t, sub) || hasSubsequence(reverse(t), reverse(sub))
  }
}
