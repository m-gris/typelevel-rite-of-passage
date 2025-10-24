package com.rockthejvm.jobsboard.http.routes

import cats.*
import cats.effect.IO
import cats.implicits.*

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.dsl.Http4sDsl

class Jobs[F[_]: Monad] private extends Http4sDsl[F] {
  // NOTE: extending Http4sDsl, allows not to have to
  //     val dsl = Http4sDsl[F]
  //     import dsl.*

  // POST /jobs?offset=x&limit=y { filters } // TODO: add query params & filters
  private val all: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    Ok("TODO: Query params & Filters")
  }

  // GET /jobs/uui
  private val get: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    Ok(s"Todo: find job with id $id")
  }

  // POST /jobs/create {<payload>}
  private val create: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root / "create" =>
    Ok("TODO: job creation route")
  }

  // PUT /jobs {<payload>}
  private val update: HttpRoutes[F] = HttpRoutes.of[F] { case PUT -> Root / UUIDVar(id) =>
    Ok(s"TODO: update job id $id")
  }

  // DELETE /jobs/uui
  private val delete: HttpRoutes[F] = HttpRoutes.of[F] { case DELETE -> Root / UUIDVar(id) =>
    Ok(s"TODO: delete job id $id")
  }

  def routes = Router(
    // NOTE: <+> is the combine operator for SemigroupK (i.e Semigroup of Kinds)
    "/jobs" -> (all <+> get <+> create <+> update <+> delete)
  )

}

object Jobs {
  def apply[F[_]: Monad] = new Jobs[F]
}
