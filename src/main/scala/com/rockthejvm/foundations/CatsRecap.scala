package com.rockthejvm.foundations

object CatsRecap {

  // Type classes refreshes for this course:

  // ***************************************************************************
  // start- FUNCTOR - map-able datastructures
  // ***************************************************************************
  trait MyFunctor[F[_]] {
    def map[A, B](container: F[A])(func: A => B): F[B]
  }
  // using Functor INSTANCES
  import cats.Functor
  import cats.instances.list.*
  val listFunctor = Functor[List] // calls the `apply` method on the Functor companion object
  // and fetches any given instance in scope
  val mappedListFunctor = listFunctor.map(List(1, 2, 3))(_ * 2) // List(2, 4, 6)

  // key: GENERALIZABLE mappable apis
  // i.e for ANY containers... not 1 for lists, 1 for options, 1 for eithers etc...
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)
  // even MORE EXPRESSIVE with
  import cats.syntax.functor.* // to expose the .map extension method on any container that has an implicit / given functor in scope
  def increment_v2[F[_]: /* CONTEXT BOUND */ Functor](container: F[Int]): F[Int] =
    container
      // extension method now available on any functor instance
      .map(_ + 1)
  // ***************************************************************************
  // end - FUNCTOR - map-able datastructures
  // ***************************************************************************

  // ***************************************************************************
  // start- APPLICATIVE - the PURE method to LIFT / WRAPP values
  // ***************************************************************************
  import cats.Applicative
  val listApplicative = Applicative[List]
  val simpleList      = listApplicative.pure(42) // List(42)
  import cats.syntax.applicative.*
  val simpleList_v2 = 42.pure[List] // List(42) !!!!
  // ***************************************************************************
  // end - APPLICATIVE - the PURE method to LIFT / WRAPP values
  // ***************************************************************************

  // ***************************************************************************
  // start- FLATMAP
  // ***************************************************************************
  trait MyFlatMap[F[_]] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }
  import cats.FlatMap
  val flatMapList = FlatMap[List]
  val flatMappedList =
    flatMapList.flatMap(List(1, 2, 3))(x => List(x, x * 10)) // List(1, 10, 2, 20, 3, 30)
  import cats.syntax.flatMap.*
  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))
  def crossProduct_v2[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  // ***************************************************************************
  // end - FLATMAP
  // ***************************************************************************

  // ***************************************************************************
  // start - MONAD - Applicative + FlatMap
  // (i.e map comes for 'free'since both already extend Functor
  // ***************************************************************************
  trait MyMonad[M[_]] extends Applicative[M] with FlatMap[M] {
    override def map[A, B](m: M[A])(f: A => B): M[B] =
      flatMap(m)(a => pure(f(a)))
  }
  import cats.Monad
  val monadList = Monad[List]
  import cats.syntax.monad.*
  def crossProduct_v3[M[_]: Monad, A, B](ma: M[A], mb: M[B]) =
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  // ***************************************************************************
  // end - MONAD - Applicative + FlatMap
  // ***************************************************************************

  // ***************************************************************************
  // Start - ERROR HANDLING - to wrap & store computations that can fail
  // i.e, listApplicative.pure(exception) will just crash !!!
  // ***************************************************************************
  trait MyApplicativeError[F[_], E] {
    def raiseError[A](error: E): F[A]
    // WARNING: Surprising at first: the E will be stored in this F[A]. How???
  }
  import cats.ApplicativeError
  import cats.syntax.either.*
  type MyError    = String
  type ErrorOr[A] = Either[MyError, A]
  val applicativeEither          = ApplicativeError[ErrorOr, String] // why String again ????
  val desiredValue: ErrorOr[Int] = applicativeEither.pure(42)
  val failedValue: ErrorOr[Int]  = applicativeEither.raiseError("Something went wrong.")
  // NOTE: we now have a datastructure that contains / signifies errors.
  // and those can be treated later, downstream, whenever we want  / need.
  // i.e we preserve the CONTROL FLOW

  trait MyMonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]
  // ***************************************************************************
  // End - ERROR Handling
  // ***************************************************************************

  def main(args: Array[String]): Unit = println("Hello world")
}
