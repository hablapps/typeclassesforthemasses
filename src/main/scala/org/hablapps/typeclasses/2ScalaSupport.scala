package org.hablapps.typeclasses

import Intro.Monoid

object ScalaSupport {

  // Scala support (using Monoid) (implicits and context bounds)
  // that type class parameters is meant to be implicit really, so you don't have to bother
  // about passing the argument manually.

  def collapse1[A](l: List[A])(implicit m: Monoid[A]) =
    l.foldLeft(m.empty)(m.combine)

  implicit val intMonoid2 = new Monoid[Int] {
    val empty = 0
    def combine(i1: Int, i2: Int) = i1 + i2
  }

  collapse1(List(1, 2, 3))

  // Now we go with context bounds

  def collapse2[A: Monoid](l: List[A]) = {
    val m = implicitly[Monoid[A]]
    l.foldLeft(m.empty)(m.combine)
  }

  collapse2(List(1, 2, 3))

  object Monoid {
    object syntax {
      def empty[A](implicit ev: Monoid[A]) = ev.empty

      implicit class MonoidOps[A](a: A)(implicit ev: Monoid[A]) {
        def |+|(other: A): A = ev.combine(a, other)
      }
    }
  }

  import Monoid.syntax._
  def collapse3[A: Monoid](l: List[A]) =
    l.foldLeft(empty[A])(_ |+| _)

  collapse3(List(1, 2, 3))

}
