package github.projects.loader

import github.projects.util.Labelled
import github.projects.data._

import cats.data.NonEmptyList
import doobie.imports._
import doobie.util.query.Query0
import doobie.util.transactor.Transactor
import fs2.Task
import fs2.interop.cats._

import scala.language.higherKinds

trait EntityLoader[A, E <: Entity, F[_]] {

  def load(as: List[A], score: Double = 1.0): F[List[EntityMatches[A, E]]]
}

object EntityLoader {

  def nelOrRecover[A, E <: Entity](names: List[String])(f: NonEmptyList[String] => Task[List[EntityMatches[A, E]]]): Task[List[EntityMatches[A, E]]] =
    NonEmptyList.fromList(names).fold(Task.now(List.empty[EntityMatches[A, E]]))(f)

  def taskLoad[A: Labelled, E <: Entity: Labelled](trans: Transactor[Task], as: List[A], query: Query0[E], score: Double): Task[List[EntityMatches[A, E]]] = 
    query
      .list
      .map(directMatch[A, E](as, _, score))
      .transact(trans)

  // TODO implement proper matching strategy
  private[loader] def directMatch[A: Labelled, E <: Entity : Labelled](as: List[A], entities: List[E], score: Double): List[EntityMatches[A, E]] = {
    val byLabel = entities.groupBy(Labelled[E].label)

    as.map { a =>
      byLabel.get(Labelled[A].label(a)).fold[EntityMatches[A, E]](Left(a))(es => Right(es.map(entity => EntityMatch(entity, score))))
    }
  }
}

import EntityLoader._
import Topic._

trait TopicEntityLoader[F[_]] extends EntityLoader[Topic, TopicEntity, F] {

  protected def query(names: NonEmptyList[String]) =
    (fr"SELECT id, name FROM topic_ontology WHERE " ++ Fragments.in(fr"name", names)).query[TopicEntity]
}

object TopicEntityLoader {

  @inline def apply[F[_]](implicit l: TopicEntityLoader[F]): TopicEntityLoader[F] = l

  def io(trans: Transactor[Task]) = new TopicEntityLoader[Task] {
    def load(as: List[Topic], score: Double = 1.0): Task[List[EntityMatches[Topic, TopicEntity]]] =
      nelOrRecover(as.map(_.name)) { names =>
        EntityLoader.taskLoad[Topic, TopicEntity](trans, as, query(names), score)
      }
  }
}

import Language._

trait LanguageEntityLoader[F[_]] extends EntityLoader[Language, LanguageEntity, F] {

  protected def query(names: NonEmptyList[String]) =
    (fr"SELECT id, name FROM language_ontology WHERE " ++ Fragments.in(fr"name", names)).query[LanguageEntity]
}

object LanguageEntityLoader {

  @inline def apply[F[_]](implicit l: LanguageEntityLoader[F]): LanguageEntityLoader[F] = l

  def io(trans: Transactor[Task]) = new LanguageEntityLoader[Task] {
    def load(as: List[Language], score: Double = 1.0): Task[List[EntityMatches[Language, LanguageEntity]]] =
      nelOrRecover(as.map(_.name)) { names =>
        EntityLoader.taskLoad[Language, LanguageEntity](trans, as, query(names), score)
      }
  }
}
