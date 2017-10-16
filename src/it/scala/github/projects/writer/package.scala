package github.projects

import doobie.imports._
import fs2.Task
import fs2.interop.cats._

package object writer {

  def MysqlTransactor() = Task.delay {
    Transactor.fromDriverManager[Task](
      "com.mysql.cj.jdbc.Driver",
      System.getenv("MYSQL_HOST"),
      System.getenv("MYSQL_USER"),
      System.getenv("MYSQL_PWD")
    )
  }

  def cleanDb(): ConnectionIO[Unit] =
    for {
      ept <- sql"DELETE FROM entitized_project_topics".update.run
      epl <- sql"DELETE FROM entitized_project_languages".update.run
      lo  <- sql"DELETE FROM language_ontology".update.run
      to  <- sql"DELETE FROM topic_ontology".update.run
      p   <- sql"DELETE FROM project".update.run
    } yield println(s"deleted ${ept + epl + lo + to + p} test data")
}
