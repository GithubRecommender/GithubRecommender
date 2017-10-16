package github.projects.writer

import github.projects.data._

import doobie.imports._
import doobie.util.transactor.Transactor
import fs2.Task
import fs2.interop.cats._

import scala.util.control.NonFatal
import scala.language.higherKinds

trait OntologyWriter[A, E <: Entity, F[_]] {

  def write(entity: A): F[E]
}

trait TopicToEntity[F[_]] extends OntologyWriter[Topic, TopicEntity, F] {

  def query(label: String) = sql"INSERT IGNORE INTO topic_ontology (label) VALUES ($label)"
}

object TopicToEntity {

  @inline def apply[F[_]](implicit t: TopicToEntity[F]): TopicToEntity[F] = t

  def io(trans: Transactor[Task]) = new TopicToEntity[Task] {
    def write(entity: Topic): Task[TopicEntity] = query(entity.name)
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .map(id => TopicEntity(id, entity.name))
      .transact(trans)
      .handleWith {
        case NonFatal(_) =>
          (for {
            id <- sql"SELECT id FROM topic_ontology WHERE label = ${entity.name}".query[Long].unique
          } yield TopicEntity(id, entity.name)).transact(trans)
      }
  }
}

trait LanguageToEntity[F[_]] extends OntologyWriter[Language, LanguageEntity, F] {

  def query(label: String) = sql"INSERT IGNORE INTO language_ontology (label) VALUES ($label)"
}

object LanguageToEntity {

  @inline def apply[F[_]](implicit t: LanguageToEntity[F]): LanguageToEntity[F] = t

  def io(trans: Transactor[Task]) = new LanguageToEntity[Task] {
    def write(entity: Language): Task[LanguageEntity] = query(entity.name)
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .map(id => LanguageEntity(id, entity.name))
      .transact(trans)
      .handleWith {
        case NonFatal(_) =>
          (for {
            id <- sql"SELECT id FROM language_ontology WHERE label = ${entity.name}".query[Long].unique
          } yield LanguageEntity(id, entity.name)).transact(trans)
      }
  }
}
