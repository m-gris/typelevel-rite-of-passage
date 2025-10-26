package com.rockthejvm.jobsboard.http

import cats.*
import cats.effect.{IO, Concurrent}
import cats.implicits.*

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.dsl.Http4sDsl

import com.rockthejvm.jobsboard.http.routes.*

/* Will unify all our routes */
class Api[F[_]: Concurrent] private {
  private val healthRoutes = Health[F].routes
  private val jobsRoutes   = Jobs[F].routes

  val endpoints = Router(
    "/api" -> (healthRoutes <+> jobsRoutes)
  )

}

object Api {
  def apply[F[_]: Concurrent] = new Api[F]
}
