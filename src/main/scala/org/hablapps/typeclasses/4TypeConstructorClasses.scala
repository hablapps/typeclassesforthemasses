package org.hablapps.typeclasses

object TypeConstructorClasses {

  // Initial version of our program
  def echo: Unit = {
    val read = scala.io.StdIn.readLine
    println(read)
    // scala.io.StdIn.readLine andThen println
  }

  // Almost everybody right now knows that's this is bad because it's coupled to a particular implementation
  // That's why interfaces were invented
  trait IO {
    def read: String
    def write(msg: String): Unit
  }
  def echo2(io: IO): Unit = {
    val read = io.read
    io.write(read)
  }
  val consoleIO = new IO {
    def read = scala.io.StdIn.readLine
    def write(msg: String) = println(msg)
  }
  echo2(consoleIO)

  // What's wrong with that approach? How can we even make this generic? This doesn't make any sense... or does it?
  val redisIO = new IO {
    import scala.concurrent.{Future, Await}
    // def read: Future[String] = ???
    def read: String = Await.result(???, ???)
    def write(msg: String): Unit = Await.result(???, ???)
  }

  // Oh oh... should I block or change the interface...? Definitively my interface MUST BE generic, wait but... that's a type class?
  trait IOAlg[F[_]] {
    def read: F[String]
    def write(msg: String): F[Unit]
  }
  object IOAlg {
    object syntax {
      def read[F[_]](implicit ev: IOAlg[F]) = ev.read
      def write[F[_]](msg: String)(implicit ev: IOAlg[F]) = ev.write(msg)
    }
  }

  import IOAlg.syntax._
  // def echo3[F[_]: IOAlg]: F[Unit] = {
  //   val r: F[String] = read
  //   write(r)
  // }
  // We have new issues, we can't combine our instructions right away. Before we could because we were using the Identity Functor,
  // which doesn't need to be unwrapped. We need MONADS! Luckily for us, Monad is just another generic interface | type class | algebra
  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](a: A): F[A]
  }
  object Monad {
    object syntax {
      implicit class MonadOps[F[_], A](fa: F[A])(implicit ev: Monad[F]) {
        def flatMap[B](f: A => F[B]) = ev.flatMap(fa)(f)
        def map[B](f: A => B) = ev.flatMap(fa)(f andThen ev.pure)
      }
    }
  }

  import Monad.syntax._
  def echo4[F[_]: IOAlg: Monad]: F[Unit] = {
    for {
      r <- read
      _ <- write(r)
    } yield ()
    // read flatMap write[F]
  }

  // We can follow using this pattern everywhere else in our code, keep adding type [constructor] classes, syntax, instances.
  // And you functions can require one or several of those type classes. You're functions are going to be as generic as they
  // need to be.

}
