package com.rockthejvm.jobsboard

import com.rockthejvm.jobsboard.http.routes.Health

import cats.*
import cats.effect.*
import cats.implicits.*

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.ember.server.EmberServerBuilder

object Application extends IOApp.Simple {

  override def run: IO[Unit] = EmberServerBuilder
    .default[IO]
    .withHttpApp(
      Health[IO].routes.orNotFound // to handle request on non-existing routes (i.e auto 404)
    )
    .build // Resource
    .use(_ => IO.println("Server Ready!") *> IO.never)

}
