package github.projects.writer

import github.projects.data._

import doobie.imports._
import fs2.Task
import fs2.interop.cats._
import org.specs2.mutable.Specification

final class OntologyWriterIoSpec extends Specification {

  sequential

  "creates new entities" >> {
    "topics" >> {
      import TopicToEntity._

      val (ins0, ins1) = (for {
        trans <- MysqlTransactor()
        ins0  <- TopicToEntity[Task](io(trans)).write(Topic("1", "a", None))
        ins1  <- TopicToEntity[Task](io(trans)).write(Topic("1", "a", None))
      } yield (ins0, ins1)).unsafeRun()

      ins0.label === "a"
      ins0.id === ins1.id
    }

    "languages" >> {
      import LanguageToEntity._

      val (ins0, ins1) = (for {
        trans <- MysqlTransactor()
        ins0  <- LanguageToEntity[Task](io(trans)).write(Language("1", "a"))
        ins1  <- LanguageToEntity[Task](io(trans)).write(Language("1", "a"))
      } yield (ins0, ins1)).unsafeRun()

      ins0.label === "a"
      ins0.id === ins1.id
    }

    step {
      MysqlTransactor().flatMap(trans => cleanDb().transact(trans)).unsafeRun()
    }
  }
}
