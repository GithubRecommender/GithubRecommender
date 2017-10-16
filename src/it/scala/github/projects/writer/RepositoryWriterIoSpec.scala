package github.projects.writer

import github.projects.data._

import doobie.imports._
import fs2.Task
import fs2.interop.cats._
import org.specs2.mutable.Specification

final class RepositoryWriterIoSpec extends Specification {

  import RepositoryWriter._

  "write repositories (with entitization) to db" >> {
    "execute" >> {
      val repo = RepositoryInfo("1", "owner", "name", "blah", Nil, Nil)

      val (id0, count0) = (for {
        trans  <- MysqlTransactor()
        topicE <- TopicToEntity[Task](TopicToEntity.io(trans)).write(Topic("1", "a", None))
        langE  <- LanguageToEntity[Task](LanguageToEntity.io(trans)).write(Language("1", "a"))
        ins    <- apply[Task](io(trans)).write(repo, List(topicE), List(langE))
      } yield ins).unsafeRun()

      count0 === 2

      val (id1, count1) = (for {
        trans  <- MysqlTransactor()
        topicE <- TopicToEntity[Task](TopicToEntity.io(trans)).write(Topic("1", "a", None))
        langE  <- LanguageToEntity[Task](LanguageToEntity.io(trans)).write(Language("1", "a"))
        ins    <- apply[Task](io(trans)).write(repo, List(topicE), List(langE))
      } yield ins).unsafeRun()

      count1 === 4
      id1 === id0
    }

    step {
      MysqlTransactor().flatMap(trans => cleanDb().transact(trans)).unsafeRun()
    }
  }
}
