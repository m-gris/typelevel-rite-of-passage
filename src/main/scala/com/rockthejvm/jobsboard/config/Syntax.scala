package com.rockthejvm.jobsboard.config

import scala.reflect.ClassTag

import cats.*
import cats.implicits.*
import cats.MonadThrow

import pureconfig.{ConfigSource, ConfigReader}
import pureconfig.error.ConfigReaderException

object Syntax {
  extension (source: ConfigSource)
    def loadF[F[_], A](using reader: ConfigReader[A], F: MonadThrow[F], tag: ClassTag[A]): F[A] =
      F.pure(source.load[A]) // F[Either[Errors, A]]
        .flatMap {
          case Left(errors)  => F.raiseError[A](ConfigReaderException(errors))
          case Right(config) => F.pure(config)

        }
}
