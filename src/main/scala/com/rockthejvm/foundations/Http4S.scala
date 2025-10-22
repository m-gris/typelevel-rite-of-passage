package com.rockthejvm.foundations

import cats.*
import cats.effect.*
import cats.implicits.*

import java.util.UUID
import java.util.Optional

// CIRCE - JSON Conversion Librabry working well with HTTP4S
// IMPORTANT: These imports must come BEFORE org.http4s.dsl.* to avoid namespace collision.
// org.http4s.dsl.* imports 'io' (org.http4s.dsl.io) which would shadow the top-level 'io' package,
// preventing resolution of io.circe.* packages.
import io.circe.generic.auto.* // macro based lib, that will generate JSON encoders-decoders for ANY CASE CLASS
import io.circe.syntax.* // extension methods...

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.typelevel.ci.CIString
import org.http4s.ember.server.EmberServerBuilder

object Http4S extends IOApp.Simple {

  // let's SIMULATE a CRUD APP
  // that we can interact with a REST API
  // (the HTTP SERVER will have `students` and `courses`)

  type CourseId = UUID
  type Student  = String

  case class Instructor(firstName: String, lastName: String)

  case class Course(
      id: CourseId,
      title: String,
      year: Int,
      students: List[Student],
      instructor: Instructor
  )

  object CourseRepository {
    // pseudo-DB
    val catsEffectCourse = Course(
      UUID.randomUUID(),
      "Cats Effect",
      2022,
      List("Marc"),
      Instructor("Daniel", "TheJVMRocker")
    )
    val courses: Map[CourseId, Course] = Map(
      catsEffectCourse.id -> catsEffectCourse
    )

    // API
    def findCourseBy(id: CourseId): Option[Course] =
      courses.get(id)

    def findCourseBy(instructor: Instructor): List[Course] =
      courses.values.filter(_.instructor == instructor).toList
  }

  // essential REST endpoints
  // GET localhost:8080/courses?instructor=Daniel%20TheJVMRocker&year=2022
  // GET localhost:8080/courses/<UUID>/students

  // for Http4S to PARSE & VALIDATE the query params
  // we need QueryParamMatchers
  // parse only
  object QueryParams:
    object InstructorFirstName extends QueryParamDecoderMatcher[String]("instructorFirstName")
    object InstructorLastName  extends QueryParamDecoderMatcher[String]("instructorLastName")
    object Year                extends OptionalValidatingQueryParamDecoderMatcher[Int]("year")

  def courseRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    // import all the operators & implicits for this [F]
    // GET, Root etc...
    import dsl.*

    HttpRoutes.of[F] {
      // takes a list of PARTIAL FUNCTIONS
      // to DECODE all the ENDPOINTS that we want to support
      case GET -> Root / "courses"
          :? QueryParams.InstructorFirstName(firstName)
          +& QueryParams.InstructorLastName(lastName)
          +& QueryParams.Year(maybeYear) =>
        val courses = CourseRepository.findCourseBy(Instructor(firstName, lastName))
        maybeYear match {
          case Some(y) =>
            y.fold(
              _ => BadRequest("Parameters 'year' is invalid"),
              year => Ok(courses.filter(_.year == year).asJson)
            )
          case None => Ok(courses.asJson)
        }

      case GET -> Root / "courses" / UUIDVar(courseId) / "students" =>
        CourseRepository.findCourseBy(courseId) match {
          case Some(course) =>
            Ok(course.students.asJson, Header.Raw(CIString("My-Custom-Header"), "rockthejvm"))
          case None => NotFound(s"No course with $courseId was found")
        }
    }

  }

  def healthEndpoint[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*
    HttpRoutes.of[F] { case GET -> Root / "health" =>
      Ok("Everything looks fine!")
    }
  }

  // combining / composing routes
  // with the SemiGroupK operator    <+>
  def allRoutes[F[_]: Monad]: HttpRoutes[F] = healthEndpoint[F] <+> courseRoutes[F]

  // combining SEVERAL GROUPS of endpoints
  // UNDER PATH PREFIXES
  def prefixedRoutes = Router(
    "/api"   -> courseRoutes[IO],  // localhost:8080/api/courses/<UUID>/students
    "/infra" -> healthEndpoint[IO] // localhost:8080/infra/health
  )

  override def run: IO[Unit] = EmberServerBuilder
    .default[IO]
    .withHttpApp(
      allRoutes[IO].orNotFound // to handle request on non-existing routes (i.e auto 404)
    )
    .build // Resource
    .use(_ => IO.println("Server Ready!") *> IO.never)

}
