package org.hablapps.typeclasses

object TypeClassPattern {

  // Type classes are made up of 5 parts

  // 1. (Abstract) Interface
  trait Order[A] {
    // Returns:
    // - a1<a2 => <0
    // - a1>a2 => >0
    // - i.o.c. => 0
    def compare(a1: A, a2: A): Int

    // 2. Derived Ops
    def gt(a1: A, a2: A): Boolean = compare(a1, a2) > 0
    def lt(a1: A, a2: A): Boolean = compare(a1, a2) < 0
    def eq(a1: A, a2: A): Boolean = compare(a1, a2) == 0
    def gteq(a1: A, a2: A): Boolean = !lt(a1, a2)
    def lteq(a1: A, a2: A): Boolean = !gt(a1, a2)

    def greater(a1: A, a2: A): A =
      if (gteq(a1, a2)) a1
      else a2

  }

  object Order {
    // 3. Instances
    def apply[A](implicit ev: Order[A]) = ev

    implicit val intInstance = new Order[Int] {
      def compare(i1: Int, i2: Int): Int = i1-i2
    }

    implicit val stringInstance = new Order[String] {
      def compare(s1: String, s2: String): Int = ???
    }

    implicit def optionInstance[A](implicit ev: Order[A]) =
      new Order[Option[A]] {
        def compare(o1: Option[A], o2: Option[A]): Int =
          (o1, o2) match {
            case (Some(a1), Some(a2)) => ev.compare(a1, a2)
            case (None, None) => 0
            case (Some(_), _) => 1
            case _ => -1
          }
      }

    implicit def listInstance[A](implicit ev: Order[A]) = new Order[List[A]] {
      def compare(xs: List[A], ys: List[A]): Int = ???
    }

    // 4. Syntax
    object syntax {
      implicit class OrderOps[A](a: A)(implicit ev: Order[A]) {
        def compareTo(other: A) = ev.compare(a, other)
        def >(other: A): Boolean = ev.gt(a, other)
        def <(other: A): Boolean = ev.lt(a, other)
        def ===(other: A): Boolean = ev.eq(a, other)
        def >=(other: A): Boolean = ev.gteq(a, other)
        def <=(other: A): Boolean = ev.lteq(a, other)
      }

      def greater[A](a1: A, a2: A)(implicit ev: Order[A]) = ev.greater(a1, a2)
    }

    // 5. Laws
    trait OrderLaws {
      import syntax._

      def antisymmetric[A: Order](a1: A, a2: A): Boolean =
        (a1 > a2) == (a2 <= a1)

      def transitive[A: Order](a1: A, a2: A, a3: A): Boolean = {
        val a1a2 = a1 > a2
        val a2a3 = a2 > a3
        val a1a3 = a1 > a3
        if (a1a2 == a2a3)
          a1a3 == a1a2
        else
          true
      }
    }
  }

  // Using the type class
  import Order.syntax._
  def quicksortList[A: Order](l: List[A]): List[A] =
    l match {
      case a :: as =>
        quicksortList(as.filter(_ < a)) ::: a :: quicksortList(as.filter(_ >= a))
      case Nil => Nil
    }
  // This could be an exercise
  def maxList[A: Order](l: List[A]): Option[A] =
    l.foldLeft(Option.empty[A]) { (acc, a) => acc.fold(Option(a))(ac => Option(greater(ac, a))) }


  trait Show[A] {
    def write(a: A): String
  }

  object Show {
    // Write the caster
    ???

    // Write an instance for Int
    implicit val intInstance: Show[Int] = ???

    // Write an instance for String
    ???

    // Write an instance for Option[A]
    ???

    // Write the syntax for write
    object syntax {
      ???
    }
  }

  // implement the next function using Show type class.
  // it must return the first element of the list as a String
  import Show.syntax._
  def writeFirst[A](l: List[A]): String = ???

}
