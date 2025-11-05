package com.rockthejvm.jobsboard.logging

import org.typelevel.log4cats.Logger

import cats.implicits.*
import cats.MonadError

object syntax {
  /* extends our F[A] effects with the Logger typeclass from typelevel.log4cats
   *  fa: F[A] because we need something more general than just IO[String]
   * as for the MonadError, it is needed both for F & E
   * */
  extension [F[_], E, A](fa: F[A])(using me: MonadError[F, E], logger: Logger[F])
    def log(success: A => String, error: E => String): F[A] =
      fa.attemptTap { /*
                      .attempt turns F[A] into F[Either[E, A]]
                      combined to
                      .tap that turns that F[Either[E, A]] into a F[B] */
        case Left(e)  => logger.error(error(e))
        case Right(a) => logger.info(success(a))
      }

    /* Logs the error-channel of an effect
     * without modifying the effect itself
     * */
    def logError(error: E => String): F[A] = fa.attemptTap {
      case Left(e) => logger.error(error(e))
      case Right(_) => ().pure[F] // don't do anything ... the pure method on unit is provided here by the MonadError Typeclass
    }

}
