package com.rockthejvm.foundations

import cats.effect.{IO, IOApp}
import scala.io.StdIn
import scala.concurrent.duration.*
import scala.util.Random
import cats.effect.kernel.Resource
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import cats.MonadError
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Fiber
import cats.effect.kernel.GenSpawn
import cats.effect.Spawn
import cats.effect.Temporal
import cats.effect.Concurrent
import cats.effect.kernel.Ref
import cats.effect.Sync
import cats.effect.kernel.Deferred
import cats.Defer
import scala.concurrent.ExecutionContext

object CatsEffects extends IOApp.Simple {

  // IO - the mother of all monads -
  // Data Structure DESCRIBING ARBITRARY COMPUTATIONS (including side-effects)

  val firstIO: IO[Int] = IO.pure(42)
  val delayedIO: IO[Int] = IO {
    /* NOTE: The 'secret' is that IO.apply takes
     * a BY NAME THUNK*/
    println("This will not be executed now")
    42
  }

  // transformations
  // map + flatMap
  val improvedMeaning0fLife = firstIO.map(_ * 2)
  val printedMeaning0fLife  = firstIO.flatMap(mol => IO(println(mol)))

  // for-comprehensions
  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _     <- IO(println(line1 + line2))
  } yield ()

  // WARNING: We should actually do it this way...
  def evaluate[A](io: IO[A]): Unit = {
    // NOTE: we need the 'platform' upon which IOs can be evaluated
    // to bring in scope the implicit IORuntime to the IO.unsafeRunSync method
    import cats.effect.unsafe.implicits.global // global: IORuntime
    val result = io.unsafeRunSync()
    println(s"The result of the effect is $result")
  }

  // raising / catching ERRORS
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper failure"))
  val dealWithIt = aFailure.handleErrorWith { case _: RuntimeException =>
    IO(println("I'm still here, no worries"))
  }

  // fibers: lightweight threads
  // DESCRIPTIONS of computations that can RUN IN PARALLEL
  val heavyDuty: IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  val sequentialCompute = for {
    x <- heavyDuty
    y <- heavyDuty
  } yield println(s"x=$x, y=$y")

  val parallelCompute = for {
    fibX <- heavyDuty.start
    fibY <- heavyDuty.start
    x    <- fibX.join
    y    <- fibY.join
  } yield println(s"x=$x, y=$y")

  val cancelledComputed = for {
    fib <- heavyDuty.onCancel(IO(println("I've been cancelled :( "))).start
    _   <- IO.sleep(500.millis) *> IO(println("Cancelling the fiber...")) *> fib.cancel
    _   <- fib.join /* WARNING: not actually 'needed', but BEST PRACTICE...
                       This 'cleans up' any ressources contained in the fiber !!!
     */
  } yield ()

  val unCancelledComputed = for {
    fib <- IO
      .uncancelable(_ => heavyDuty.onCancel(IO(println("I've been cancelled (won't happen)"))))
      .start // WARNING: start is invoked on IO.uncancellabe not on what's "inside"!!!
    // .........i.e, this WONT WORK: IO.uncancellable(someIO.start)
    _ <- IO.sleep(500.millis) *> IO(
      println("(Trying to) Cancelling the fiber...")
    ) *> fib.cancel // NOTE: will NOT cancel...
    _ <- fib.join
  } yield ()

  // RESSOURCES - (auto-clean-up/teardown)
  val RIGHT_HERE =
    "/Users/marc/DATA_PROG/SCALA/rock-the-jvm/typelevel-rite-of-passage/src/main/scala/com/rockthejvm/foundations/CatsEffects.scala"
  val readingResource =
    Resource
      .make {
        // ACQUIRE
        IO(println("Acquiring Resource")) *> IO(scala.io.Source.fromFile(RIGHT_HERE))
      } {
        // RELEASE
        source => IO(println("Releasing Resource")) *> IO(source.close())
      }
  val readingEffect = readingResource.use(source => IO(source.getLines().take(20).foreach(println)))

  val copiedFileResource = Resource.make {
    IO(new PrintWriter(new FileWriter(new File("src/main/resources/dumpedFile.scala"))))
  } { writer =>
    IO(println("closing duplicated file")) *> IO(writer.close())
  }

  val composedResources = for {
    source      <- readingResource
    destination <- copiedFileResource
  } yield (source, destination)

  val copiedFileEffect = composedResources.use { (src, dst) =>
    IO(src.getLines().foreach(dst.println))
  }

  // ABSTRACT KINDS OF COMPUTATIONS
  //
  // cancellable computations => MonadCancel
  trait MyMonadCancel[F[_], E] extends MonadError[F, E] {
    trait CancellationFlagResetter {
      def apply[A](f: F[A]): F[A] // an F[A] with its cancellation flag resetted
    }
    def canceled: F[Unit]
    def uncancellable[A](poll: CancellationFlagResetter => F[A]): F[A]
  }
  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val uncancellable = monadCancelIO.uncancelable(_ => IO(42)) // Same as IO.uncancellable(42)

  // Spawn => the ability to create FIBERS
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    // NOTE:
    // start() is the FUNDAMENTAL API of GenSpawn
    // it returns F[Fiber[F,E,A]]
    // i.e
    // an effect F
    // that wraps a Fiber
    // that has 3 type arguments
    // F -> the effect type that the fiber will end up evaluating
    // E -> the Error Type
    // A -> the final VALUE type the effect
    def start[A](fa: F[A]): F[Fiber[F, E, A]]
    // over secondary API (i.e derivable from start())
    // NEVER, CEDE, RACEPAIR etc...
  }

  // "fixing" the ERROR TYPE to Throwable
  // (i.e, in real life, JVM Computations fail with a Throwable)
  // (this is basically just a type alias... no specific API)
  trait MySpawn[F[_]] extends GenSpawn[F, Throwable]

  // we can of course FETCH IMPLICIT INSTANCES of Spawn for the IO type
  val spawnIO = Spawn[IO]
  // NOTE: IO[Fiber]
  // i.e does not create the fiber
  // but DESCRIBES THE EFFECT OF CREATING A FIBER...
  val aFiber: IO[Fiber[IO, Throwable, Int]] = spawnIO.start(delayedIO) // same as delayedIO.start

  // CONCURRENCY PRIMITIVES: atomic references + promises
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  // Temporal Typeclass
  // the ABILITY OF SUSPENDING computations for a given time
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  // Sync typeclass
  // the ability the SUSPEND SYNCRHONOUS arbitrary expressions/computations in an effect
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {

    def delay[A](expression: => A): F[A]

    // BLOCKING: runs the effect on a DEDICATED THREADPOOL
    // to avoid STARVING the ORIGINAL THREADPOOL
    def blocking[A](expression: => A): F[A]
  }

  // Async typeclass (the most powerful one in cats effect)
  // the ABILITY TO SUSPEND ASYNC COMPUTATIONS
  // (i.e computations that runs on OTHER - non cats-effect managed - THREADPOOLS)
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    def executionContext: F[ExecutionContext]
    def async[A](
      callback: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]
    ): F[A]
  }

  def old_style_scala_main(args: Array[String]): Unit =
    evaluate(delayedIO)
    evaluate(smallProgram())

  // WARNING: the API for IOApp.Simple
  override def run: IO[Unit] =
    improvedMeaning0fLife >> IO.unit
    copiedFileEffect

}
