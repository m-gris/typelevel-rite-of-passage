package com.rockthejvm.jobsboard.http.routes

import java.util.UUID

import scala.collection.mutable

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*

import cats.*
import cats.effect.{IO, Concurrent}
import cats.implicits.*

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.dsl.Http4sDsl

import com.rockthejvm.jobsboard.domain.job.*
import com.rockthejvm.jobsboard.http.responses.*
import fs2.io.Watcher.Event.Created

class Jobs[F[_]: Concurrent] private extends Http4sDsl[F] {
  // NOTE: extending Http4sDsl, allows not to have to
  //     val dsl = Http4sDsl[F]
  //     import dsl.*

  // pseudo database for now:
  private val database = mutable.Map[UUID, Job]()

  // POST /jobs?offset=x&limit=y { filters } // TODO: add query params & filters
  private val all: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    Ok(database.values)
  }

  // GET /jobs/uui
  private val get: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    database.get(id) match {
      case Some(job) => Ok(job)
      case None      => NotFound(FailureResponse(s"Job $id not found."))

    }
  }

  // POST /jobs/create {<payload>}
  private def createJob(
      jobInfo: JobInfo
  ): F[Job] = // the effect F is dependent upon the business logic
    Job(
      id = UUID.randomUUID(),
      date = System.currentTimeMillis(),
      info = jobInfo,
      poster = JobPoster(email = "TODO@ouremail.com"),
      is_active = true
    ).pure[F]

  private val create: HttpRoutes[F] = HttpRoutes.of[F] { case request @ POST -> Root / "create" =>
    for {
      // parse payload with CIRCE as a JobInfo
      // NOTE: .as[] is an extension method from Circe,
      // allowing to parse the payload into an F[JobInfo]
      jobInfo  <- request.as[JobInfo]
      job      <- createJob(jobInfo)
      response <- Created(job.id)
    } yield response
  }

  // PUT /jobs {<payload>}
  private val update: HttpRoutes[F] = HttpRoutes.of[F] { case request @ PUT -> Root / UUIDVar(id) =>
    database.get(id) match {
      case Some(job) => for {
        jobInfo <- request.as[JobInfo]
        _       <- database.put(id, job.copy(info=jobInfo)).pure[F]
        response <- Ok()
      } yield response
      case None => NotFound(FailureResponse(s"Job $id not found."))
    }
  }

  // DELETE /jobs/uui
  private val delete: HttpRoutes[F] = HttpRoutes.of[F] { case request @ DELETE -> Root / UUIDVar(id) =>
    database.get(id) match {
      case Some(job) => for {
        _ <- database.remove(id).pure[F]
        response <- Ok()
      } yield response
      case None => NotFound(FailureResponse(s"Job $id not found."))
    }
  }

  def routes = Router(
    // NOTE: <+> is the combine operator for SemigroupK (i.e Semigroup of Kinds)
    "/jobs" -> (all <+> get <+> create <+> update <+> delete)
  )

}

object Jobs {
  def apply[F[_]: Concurrent] = new Jobs[F]
}
