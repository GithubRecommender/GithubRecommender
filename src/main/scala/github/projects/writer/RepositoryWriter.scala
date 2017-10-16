package github.projects.writer

import github.projects.data._

import cats.Applicative
import cats.instances.list._
import doobie.imports._
import doobie.util.transactor.Transactor
import fs2.Task
import fs2.interop.cats._

import scala.language.higherKinds

trait RepositoryWriter[F[_]] {

  def write(repo: RepositoryInfo, entTopics: List[TopicEntity], entLanguages: List[LanguageEntity]): F[(Long, Int)]
}

object RepositoryWriter {

  @inline def apply[F[_]](implicit r: RepositoryWriter[F]): RepositoryWriter[F] = r

  def io(trans: Transactor[Task]) = new RepositoryWriter[Task] {

    private final val InsertTopics = "INSERT INTO entitized_project_topics (project_id, topic_id) VALUES (?, ?)"
    private final val InsertLangs  = "INSERT INTO entitized_project_languages (project_id, language_id) VALUES (?, ?)"

    def write(repo: RepositoryInfo, entTopics: List[TopicEntity], entLanguages: List[LanguageEntity]): Task[(Long, Int)] =
      (for {
        repoIdO <- sql"SELECT id FROM project WHERE gh_id = ${repo.id}".query[Long].option
        repoId  <- {
          if (repoIdO.isEmpty) {
            sql"INSERT INTO project (gh_id, owner, name, description) VALUES (${repo.id}, ${repo.owner}, ${repo.name}, ${repo.description})"
              .update
              .withUniqueGeneratedKeys[Long]("id")
          }
          else
            Applicative[ConnectionIO].pure(repoIdO.get)
        }

        delTopicCount <- sql"DELETE FROM entitized_project_topics WHERE project_id = $repoId".update.run
        insTopicCount <- Update[(Long, Long)](InsertTopics).updateMany(entTopics.map(t => (repoId, t.id)))

        delLangCount <- sql"DELETE FROM entitized_project_languages WHERE project_id = $repoId".update.run
        insLangCount <- Update[(Long, Long)](InsertLangs).updateMany(entLanguages.map(t => (repoId, t.id)))
      } yield (repoId, delTopicCount + insTopicCount + delLangCount + insLangCount)).transact(trans)
  }
}
