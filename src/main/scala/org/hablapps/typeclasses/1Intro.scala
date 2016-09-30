package org.hablapps.typeclasses

object Intro {

  def addUp(numbers: List[Int]): Int =
    numbers match {
      case Nil => 0
      case i :: is => i + addUp(is)
    }

  def collapse[A](l: List[A])(empty: A, combine: (A, A) => A): A =
    l match {
      case Nil => empty
      case i :: is => combine(i, collapse(is)(empty, combine))
    }

  trait Monoid[A] {
    val empty: A
    def combine(a1: A, a2: A): A
  }

  def collapse2[A](l: List[A])(m: Monoid[A]): A =
    l.foldLeft(m.empty)(m.combine)

  val intMonoid = new Monoid[Int] {
    val empty = 0
    def combine(i1: Int, i2: Int) = i1 + i2
  }

  collapse2(List(1, 2, 3))(intMonoid)

}
