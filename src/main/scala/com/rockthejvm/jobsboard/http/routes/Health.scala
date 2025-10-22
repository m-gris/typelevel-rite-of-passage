package com.rockthejvm.jobsboard.http.routes

import cats.effect.IO
import cats.Monad

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.dsl.Http4sDsl

class Health[F[_]: Monad] private extends Http4sDsl[F] {
  // NOTE: extending Http4sDsl, allows not to have to
  //     val dsl = Http4sDsl[F]
  //     import dsl.*
  def check: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root =>
    Ok("Everything looks fine!")
  }

  def routes = Router(
    "health" -> check
  )

}

object Health {
  def apply[F[_]: Monad] = new Health[F]
}
