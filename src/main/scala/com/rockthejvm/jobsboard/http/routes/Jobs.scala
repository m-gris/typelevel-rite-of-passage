package com.rockthejvm.jobsboard.http.routes

import java.util.UUID

import scala.collection.mutable

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*

import cats.*
import cats.effect.{IO, Concurrent}
import cats.implicits.*

import org.typelevel.log4cats.Logger

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.dsl.Http4sDsl

import com.rockthejvm.jobsboard.domain.job.*
import com.rockthejvm.jobsboard.http.responses.*
import com.rockthejvm.jobsboard.logging.syntax.*

// NOTE:
// CHAINED CONTEXT BOUNDS for Jobs[F[_]: Concurrent: Logger]
// EQUIVALENT TO
// class Jobs[F[_]](implicit ev1: Concurrent[F], ev2: Logger[F])
// i.e  F needs to have both a Concurrent instance and a Logger instance available implicitly
class Jobs[F[_]: Concurrent: Logger] private extends Http4sDsl[F] {
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
      isActive = true
    ).pure[F]

  private val create: HttpRoutes[F] = HttpRoutes.of[F] { case request @ POST -> Root / "create" =>
    // NOTE: below we parse the payload with CIRCE as a JobInfo
    // using .as[] which is an extension method from Circe,
    // allowing to parse the payload into an F[JobInfo]
    for {
      _        <- Logger[F].info("Trying to add job")
      jobInfo  <- request.as[JobInfo].logError(e => s"Parsing payload faied: $e")
      _        <- Logger[F].info(s"Parsed jobInfo $jobInfo")
      job      <- createJob(jobInfo)
      _        <- database.put(job.id, job).pure[F]
      _        <- Logger[F].info(s"Created job $job")
      response <- Created(job.id)
    } yield response
  }

  // PUT /jobs {<payload>}
  private val update: HttpRoutes[F] = HttpRoutes.of[F] { case request @ PUT -> Root / UUIDVar(id) =>
    database.get(id) match {
      case Some(job) =>
        for {
          _        <- Logger[F].info(s"Trying to update job $id")
          jobInfo  <- request.as[JobInfo].logError(e => s"Parsing payload faied: $e")
          _        <- Logger[F].info(s"Parsed jobInfo $jobInfo")
          _        <- database.put(id, job.copy(info = jobInfo)).pure[F]
          _        <- Logger[F].info(s"Updated job $job")
          response <- Ok()
        } yield response
      case None => NotFound(FailureResponse(s"Job $id not found."))
    }
  }

  // DELETE /jobs/uui
  private val delete: HttpRoutes[F] = HttpRoutes.of[F] {
    case request @ DELETE -> Root / UUIDVar(id) =>
      database.get(id) match {
        case Some(job) =>
          for {
            _        <- Logger[F].info(s"Trying to delete job $id")
            _        <- database.remove(id).pure[F].logError(e => s"Parsing payload faied: $e")
            _        <- Logger[F].info(s"Deleted job $id")
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
  def apply[F[_]: Concurrent: Logger] = new Jobs[F]
}
