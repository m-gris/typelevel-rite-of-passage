package com.rockthejvm.foundations

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.MonadCancelThrow
import doobie.implicits.* // the sql string interpolator (amongst other things)
import doobie.util.transactor.Transactor
import cats.effect.kernel.Resource
import doobie.util.ExecutionContexts
import doobie.hikari.HikariTransactor

object Doobie extends IOApp.Simple {

  // to INTERACT with a DB
  // we need a TRANSACTOR
  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    // the minimum / required ARGUMENTS
    "org.postgresql.Driver", // the JDBC Driver that will interact with the DB
    "jdbc:postgresql://localhost:5433/demo",
    "docker", // username
    "docker"  //  password
  )

  // the transactor allows to:
  // Execute Queries AS AN EFFECT
  case class Student(id: Int, name: String)

  def findAllStudents: IO[List[Student]] = {
    sql"SELECT * FROM students"
      .query[Student] // the TYPE OF EACH ROW
      .to[List]       // .to[F] -> the COLLECTION STRATEGY for each the row returned by the query.
      .transact(xa)
  }

  def findAllStudentNames: IO[List[String]] = {
    sql"""SELECT name FROM students"""
      .query[String] // each row as a String
      .to[List]      // gathered in a List
      .transact(xa)
  }

  def insert(student: Student): IO[Int] = { // IO[Int] because we get the number of Rows impacted
    sql"INSERT INTO students(id, name) VALUES (${student.id}, ${student.name})".update.run
      .transact(xa)
  }

  def findAllStudentsByInitial(letter: String): IO[List[Student]] = {

    // building (programmatically) queries

    val select = fr"select id, name"
    val from   = fr"from students"
    val where  = fr"where left(name, 1) = $letter"

    val statement = select ++ from ++ where

    statement
      .query[Student]
      .to[List]
      .transact(xa)

  }

  // IDIOMATIC TYPELEVEL CODE ORGANIZATION
  // the REPOSITORY is a TYPECLASS with some CAPABILITIES
  // to deals with certaint ENTITIES (Student in this case)
  trait Students[F[_]] { // F is the EFFECT TYPE
    def findById(id: Int): F[Option[Student]]
    def findAll: F[List[Student]]
    def create(name: String): F[Int]
  }
  // NOTE: This uses the TAGLESS FINAL approach:
  // i.e, a TYPECLASS
  // that DESCRIBRES some CAPABILITIES
  // in terms of a GENERAL EFFECT TYPE

  object Students {
    def make[F[_]: MonadCancelThrow](xa: Transactor[F]): Students[F] = new Students[F] {
      def findById(id: Int): F[Option[Student]] =
        sql"SELECT id, name FROM students WHERE id = $id"
          .query[Student]
          .option
          .transact(xa)

      def findAll: F[List[Student]] =
        sql"SELECT * FROM students"
          .query[Student]
          .to[List]
          .transact(xa)

      def create(name: String): F[Int] =
        sql"INSERT INTO students(name) VALUES($name)"
          .update
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
    }
  }

  val postgresResource: Resource[IO, Transactor[IO]] = for {
    // first - create a THREADPOOL (the transactor made earlier is single threaded)
    exeContext <- ExecutionContexts.fixedThreadPool[IO](16)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql://localhost:5433/demo",
      "docker",
      "docker",
      exeContext
    )
  } yield xa

  val program = postgresResource.use { xa =>
    val studentsRepo = Students.make[IO](xa)
    for {
      id     <- studentsRepo.create("Hector")
      hector <- studentsRepo.findById(id)
      _      <- IO(println(s"Hector in DB: $hector"))
    } yield hector
  }

  override def run: IO[Unit] = program.void

}
