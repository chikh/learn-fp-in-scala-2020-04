package dataStructures

import scala.util.control.NonFatal

sealed trait Option[+A]
final case class Some[A](v: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def flatMap[A, B](o: Option[A])(f: A => Option[B]): Option[B] =
    o match {
      case None    => None
      case Some(v) => f(v)
    }

  def map[A, B](o: Option[A])(f: A => B): Option[B] =
    flatMap(o)(v => Some(f(v)))

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    flatMap(oa)(a => map(ob)(f(a, _)))

  def sequence[A](os: List[Option[A]]): Option[List[A]] =
    traverse(os)(identity)
  /*
  def sequence[A](os: List[Option[A]]): Option[List[A]] =
    List.foldRightViaLeft(os)(Some(Nil): Option[List[A]]) {
      (o: Option[A], oas: Option[List[A]]) => map2(o, oas)(Cons(_, _))
    }*/

  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    /*sequence(List.map(l)(f))*/
    List.foldRightViaLeft(l)(Some(Nil): Option[List[B]]) { (a, ob) =>
      map2(ob, f(a))((bs, b) => Cons(b, bs))
    }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case NonFatal(_) => None }
}
