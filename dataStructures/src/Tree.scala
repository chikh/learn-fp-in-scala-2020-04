package dataStructures

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def size(t: Tree[_]): Int = fold(t)(_ => 1)((l, r) => l + r + 1)

  def maximum[A](t: Tree[A])(implicit o: Ordering[A]): A =
    fold(t)(identity)(o.max(_, _))

  def depth(t: Tree[_]): Int = fold(t)(_ => 0)((l, r) => (l max r) + 1)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))

  def fold[A, B](t: Tree[A])(fl: A => B)(fb: (B, B) => B): B =
    t match {
      case Leaf(v)      => fl(v)
      case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))
    }
}
