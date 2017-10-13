package github.projects.reader

import github.projects.util.Labelled
import github.projects.data._

import cats.data.NonEmptyList
import doobie.imports._
import doobie.util.query.Query0
import doobie.util.transactor.Transactor
import fs2.Task
import fs2.interop.cats._

import scala.language.higherKinds

trait EntityReader[A, E <: Entity, F[_]] {

  def read(as: List[A], score: Double = 1.0): F[List[EntityMatches[A, E]]]
}

object EntityReader {

  def nelOrRecover[A, E <: Entity](names: List[String])(f: NonEmptyList[String] => Task[List[EntityMatches[A, E]]]): Task[List[EntityMatches[A, E]]] =
    NonEmptyList.fromList(names).fold(Task.now(List.empty[EntityMatches[A, E]]))(f)

  def taskLoad[A: Labelled, E <: Entity: Labelled](trans: Transactor[Task], as: List[A], query: Query0[E], score: Double): Task[List[EntityMatches[A, E]]] = 
    query
      .list
      .map(directMatch[A, E](as, _, score))
      .transact(trans)

  // TODO implement proper matching strategy
  private[reader] def directMatch[A: Labelled, E <: Entity : Labelled](as: List[A], entities: List[E], score: Double): List[EntityMatches[A, E]] = {
    val byLabel = entities.groupBy(Labelled[E].label)

    as.map { a =>
      byLabel.get(Labelled[A].label(a)).fold[EntityMatches[A, E]](Left(a))(es => Right(es.map(entity => EntityMatch(entity, score))))
    }
  }
}

import EntityReader._
import Topic._

trait TopicEntityReader[F[_]] extends EntityReader[Topic, TopicEntity, F] {

  protected def query(names: NonEmptyList[String]) =
    (fr"SELECT id, name FROM topic_ontology WHERE " ++ Fragments.in(fr"name", names)).query[TopicEntity]
}

object TopicEntityReader {

  @inline def apply[F[_]](implicit l: TopicEntityReader[F]): TopicEntityReader[F] = l

  def io(trans: Transactor[Task]) = new TopicEntityReader[Task] {
    def read(as: List[Topic], score: Double = 1.0): Task[List[EntityMatches[Topic, TopicEntity]]] =
      nelOrRecover(as.map(_.name)) { names =>
        EntityReader.taskLoad[Topic, TopicEntity](trans, as, query(names), score)
      }
  }
}

import Language._

trait LanguageEntityReader[F[_]] extends EntityReader[Language, LanguageEntity, F] {

  protected def query(names: NonEmptyList[String]) =
    (fr"SELECT id, name FROM language_ontology WHERE " ++ Fragments.in(fr"name", names)).query[LanguageEntity]
}

object LanguageEntityReader {

  @inline def apply[F[_]](implicit l: LanguageEntityReader[F]): LanguageEntityReader[F] = l

  def io(trans: Transactor[Task]) = new LanguageEntityReader[Task] {
    def read(as: List[Language], score: Double = 1.0): Task[List[EntityMatches[Language, LanguageEntity]]] =
      nelOrRecover(as.map(_.name)) { names =>
        EntityReader.taskLoad[Language, LanguageEntity](trans, as, query(names), score)
      }
  }
}
