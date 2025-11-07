package com.rockthejvm.jobsboard.algebras

import java.util.UUID

import cats.*
import cats.effect.*
import cats.effect.kernel.Resource
import cats.implicits.*

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*

import com.rockthejvm.jobsboard.domain.job.*

trait Jobs[F[_]] {
  def create(poster: JobPoster, info: JobInfo): F[UUID]
  def all: F[List[Job]]
  def find(id: UUID): F[Option[Job]]
  def update(id: UUID, info: JobInfo): F[Option[Job]]
  def delete(id: UUID): F[Int]
}

class LiveJobs[F[_]: MonadCancelThrow] private (xa: Transactor[F]) extends Jobs[F] {

  override def create(poster: JobPoster, info: JobInfo): F[UUID] =
    sql"""
      INSERT INTO jobs
          (date,
          company,
          title,
          description,
          country,
          city,
          salary_min,
          salary_max,
          currency,
          external_url,
          is_remote,
          tags,
          image_url,
          seniority,
          other,
          poster_email,
          is_active)
          VALUES (
            ${System.currentTimeMillis()},
            ${info.company},
            ${info.title},
            ${info.description},
            ${info.location.country},
            ${info.location.city},
            ${info.salary.min},
            ${info.salary.max},
            ${info.salary.currency},
            ${info.externalUrl},
            ${info.isRemote},
            ${info.tags},
            ${info.imageUrl},
            ${info.seniority},
            ${info.other},
            ${poster.email},
            false -- not active by default...
            )
      """
      // Doobie chain: Fragment -> .update -> Update0 -> .withUniqueGeneratedKeys -> ConnectionIO[UUID]
      // .update: converts INSERT/UPDATE/DELETE Fragment to Update0 (modifies data)
      // .withUniqueGeneratedKeys[A]("col"): executes and returns generated column value
      .update
      .withUniqueGeneratedKeys[UUID]("id")
      .transact(xa) // transact: runs ConnectionIO[A] in effect F[A] using Transactor

  override def all: F[List[Job]] =
    sql"""
      SELECT
        --
        id,
        date,
        -- JOB INFO
        company,
        title,
        description,
        country,
        city,
        salary_min,
        salary_max,
        currency,
        external_url,
        is_remote,
        tags,
        image_url,
        seniority,
        other,
        -- JOB POSTER
        poster_email,
        --
        is_active
      FROM jobs
      """
      // Doobie chain: Fragment -> .query[A] -> Query0[A] -> .to[F] -> ConnectionIO[F[A]]
      // .query[A]: converts SELECT Fragment to Query0[A] (reads data, decodes to type A)
      // .to[F]: accumulates results into collection F (List, Vector, etc.)
      .query[Job]
      .to[List]
      .transact(xa) // transact: runs ConnectionIO[A] in effect F[A] using Transactor

  override def find(id: UUID): F[Option[Job]] =
    sql"""
      SELECT
        id,
        date,
        company,
        title,
        description,
        country,
        city,
        salary_min,
        salary_max,
        currency,
        external_url,
        is_remote,
        tags,
        image_url,
        seniority,
        other,
        poster_email,
        is_active
      FROM jobs
      WHERE id = ${id}
      """
      // Doobie chain: Fragment -> .query[A] -> Query0[A] -> .option -> ConnectionIO[Option[A]]
      // .query[A]: converts SELECT Fragment to Query0[A] (reads data, decodes to type A)
      // .option: expects 0 or 1 row, returns Option[A] (fails if >1 row)
      .query[Job]
      .option
      .transact(xa) // transact: runs ConnectionIO[A] in effect F[A] using Transactor

  override def update(id: UUID, info: JobInfo): F[Option[Job]] =
    sql"""
    UPDATE jobs
    SET
      company=${info.company},
      title=${info.title},
      description=${info.description},
      country=${info.location.country},
      city=${info.location.city},
      salary_min=${info.salary.min},
      salary_max=${info.salary.max},
      currency=${info.salary.currency},
      external_url=${info.externalUrl},
      is_remote=${info.isRemote},
      tags=${info.tags},
      image_url=${info.imageUrl},
      seniority=${info.seniority},
      other=${info.other}
    WHERE id = ${id}
  """
  // Doobie chain: Fragment -> .update -> Update0 -> .run -> ConnectionIO[Int]
  // .update: converts UPDATE Fragment to Update0 (modifies data)
  // .run: executes and returns number of rows affected (Int)
  .update
  .run
  .transact(xa) // transact: runs ConnectionIO[Int] in effect F[Int] using Transactor
  .flatMap{_ => find(id)} // return the updated job

  override def delete(id: UUID): F[Int] =
    sql"""
      DELETE FROM jobs
      WHERE id = ${id}
    """
    // Doobie chain: Fragment -> .update -> Update0 -> .run -> ConnectionIO[Int]
    // .update: converts DELETE Fragment to Update0 (modifies data)
    // .run: executes and returns number of rows affected (Int)
    .update
    .run
    .transact(xa) // transact: runs ConnectionIO[Int] in effect F[Int] using Transactor

}

object LiveJobs {

  // allows DOOBIE to 'map' it tuples to our desired type
  given jobRead: Read[Job] = Read[
    (
        UUID,                 // id
        Long,                 // date
        String,               // company
        String,               // title
        String,               // description
        Option[String],       // country
        Option[String],       // city
        Option[Int],          // min salary
        Option[Int],          // max salary
        Option[String],       // currency
        String,               // externalUrl
        Boolean,              // isRemote
        Option[List[String]], // tags
        Option[String],       // imageUrl
        Option[String],       // seniority
        Option[String],       // other
        String,               // poster_email
        Boolean               // isActive
    )
  ].map {
    case (
          id: UUID,
          date: Long,
          company: String,
          title: String,
          description: String,

          /*The Option[A] here being subject to a pattern match
           * see their type erased at runtime.
           * We must therefore annotate those as 'unchecked'
           * */
          country: Option[String] @unchecked,
          city: Option[String] @unchecked,
          minSalary: Option[Int] @unchecked,
          maxSalary: Option[Int] @unchecked,
          currency: Option[String] @unchecked,
          externalUrl: String,
          isRemote: Boolean,
          tags: Option[List[String]] @unchecked,
          imageUrl: Option[String] @unchecked,
          seniority: Option[String] @unchecked,
          other: Option[String] @unchecked,
          poster_email: String,
          isActive: Boolean
        ) =>
      Job(
        id = id,
        date = date,
        info = JobInfo(
          company = company,
          title = title,
          description = description,
          location = Location(
            country = country,
            city = city
          ),
          salary = Salary(
            min = minSalary,
            max = maxSalary,
            currency = currency
          ),
          externalUrl = externalUrl,
          isRemote = isRemote,
          tags = tags,
          imageUrl = imageUrl,
          seniority = seniority,
          other = other
        ),
        poster = JobPoster(poster_email),
        isActive = isActive
      )
  }
  // the creation of LiveJobs is itself an effectuful operation
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): F[LiveJobs[F]] =
    new LiveJobs[F](xa).pure[F]
}
