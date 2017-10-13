package github.projects.writer

import github.projects.data._

import doobie.h2.imports._
import fs2.Task
import fs2.interop.cats._
import org.specs2.mutable.Specification

final class OntologyWriterIoSpec extends Specification {

  "creates new entities" >> {
    "topics" >> {
      import TopicToEntity._

      val result = for {
        trans <- H2Transactor[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1;MODE=MySQL;INIT=RUNSCRIPT FROM 'src/sql/create_tables.sql'", "sa", "")
        ins   <- TopicToEntity[Task](io(trans)).write(Topic("1", "a", None))
      } yield ins

      result.unsafeRun() === TopicEntity(1L, "a")
    }

    "languages" >> {
      import LanguageToEntity._

      val result = for {
        trans <- H2Transactor[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1;MODE=MySQL;INIT=RUNSCRIPT FROM 'src/sql/create_tables.sql'", "sa", "")
        ins   <- LanguageToEntity[Task](io(trans)).write(Language("1", "a"))
      } yield ins

      result.unsafeRun() === LanguageEntity(1L, "a")
    }
  }
}
