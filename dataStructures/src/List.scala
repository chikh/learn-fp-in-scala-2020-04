package dataStructures

import scala.annotation.tailrec

sealed trait List[+A]

final case class Cons[A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
    else
      l match {
        case Nil        => throw new NoSuchElementException("can't drop")
        case Cons(h, t) => drop(t, n - 1)
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
}
