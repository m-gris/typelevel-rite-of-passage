package com.rockthejvm.jobsboard

import com.rockthejvm.jobsboard.http
import com.rockthejvm.jobsboard.config.Syntax.*

import cats.*
import cats.effect.*
import cats.implicits.*

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.ember.server.EmberServerBuilder

import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderFailures
import com.rockthejvm.jobsboard.config.EmberConfig

object Application extends IOApp.Simple {

  override def run: IO[Unit] = ConfigSource.default.loadF[IO, EmberConfig].flatMap { config =>
    EmberServerBuilder
      .default[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(
        http.Api[IO].endpoints.orNotFound // to handle request on non-existing routes (i.e auto 404)
      )
      .build // Resource
      .use(_ => IO.println("Server Ready!") *> IO.never)
  }

}
