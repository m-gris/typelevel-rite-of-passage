package com.rockthejvm.jobsboard.playground

import cats.effect.*
import doobie.*
import doobie.implicits.*
import doobie.util.*
import doobie.hikari.HikariTransactor
import com.rockthejvm.jobsboard.domain.job.Job
import com.rockthejvm.jobsboard.domain.job.JobPoster
import com.rockthejvm.jobsboard.domain.job.JobInfo
import com.rockthejvm.jobsboard.domain.job.Salary
import com.rockthejvm.jobsboard.algebras.LiveJobs
import scala.io.StdIn
import com.rockthejvm.jobsboard.domain.job.Location

object JobsPlayground extends IOApp.Simple {

  val TABLE_NAME = "board"
  val postgresResource: Resource[IO, HikariTransactor[IO]] = for {
    ec <- ExecutionContexts.fixedThreadPool(32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      driverClassName = "org.postgresql.Driver",
      url = s"jdbc:postgresql://localhost:5433/$TABLE_NAME",
      user = "docker", // as per docker-compose.yml
      pass = "docker", // as per docker-compose.yml
      connectEC = ec
    )
  } yield xa

  val jobInfo = JobInfo(
    company = "Rock The JVM",
    title = "Looking for an FP Rock Start",
    description = "The best you could find",
    location = Location(country = None, city = None),
    salary = Salary(min = None, max = None, currency = None),
    externalUrl = "rockthejvm.com",
    isRemote = true,
    tags = Some(List("awesome", "fp", "scala")),
    imageUrl = None,
    seniority = None,
    other = None
  )

  override def run: IO[Unit] = postgresResource.use { xa =>
    for {
      jobs      <- LiveJobs.make(xa)
      _         <- IO(println("Ready. Next...")) *> IO(StdIn.readLine)
      id        <- jobs.create(JobPoster("daniel@rockthejvm.com"), jobInfo)
      _         <- IO(println("Next...")) *> IO(StdIn.readLine)
      list      <- jobs.all
      _         <- IO(println(s"All jobs: $list. Next...")) *> IO(StdIn.readLine)
      _         <- jobs.update(id, jobInfo.copy(title = "Software rockstar"))
      newJob    <- jobs.find(id)
      _         <- IO(println(s"New job: $newJob. Next...")) *> IO(StdIn.readLine)
      _         <- jobs.delete(id)
      listAfter <- jobs.all
      _         <- IO(println(s"Deleted job. List now: $listAfter. Next...")) *> IO(StdIn.readLine)
    } yield ()
  }

}
