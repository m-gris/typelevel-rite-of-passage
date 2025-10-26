package com.rockthejvm.jobsboard.domain

import java.util.UUID

object job {
  case class Job(
      id: UUID,
      date: Long,
      info: JobInfo,
      poster: JobPoster,
      is_active: Boolean
  )
  case class JobInfo(
      company: String,
      title: String,
      description: String,
      location: Location,
      salary: Salary,
      externalUrl: String,
      is_remote: Boolean,
      tags: Option[List[String]],
      image_url: Option[String],
      seniority: Option[String],
      other: Option[String]
  )

  object JobInfo {
    val empty: JobInfo = JobInfo(
      company = "",
      title = "",
      description = "",
      location = Location(country = None, city = None),
      salary = Salary(min = None, max = None, currency = None),
      externalUrl = "",
      is_remote = false,
      tags = None,
      image_url = None,
      seniority = None,
      other = None
    )
  }

  case class Salary(min: Option[Int], max: Option[Int], currency: Option[String])
  case class Location(country: Option[String], city: Option[String])
  case class JobPoster(email: String)
}
