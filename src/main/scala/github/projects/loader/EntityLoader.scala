package github.projects.loader

import github.projects.util.Labelled
import github.projects.data._

import fs2.{Task, Strategy}
import doobie.imports._
import doobie.util.transactor.Transactor
import cats.data._
import fs2.interop.cats._

import scala.language.higherKinds

trait EntityLoader[F[_]] {

  def topicEntities(topics: Seq[Topic]): F[Map[Topic, Seq[EntityMatch[TopicEntity]]]]
  def languageEntities(languages: Seq[Language]): F[Map[Language, Seq[EntityMatch[LanguageEntity]]]]
}

object EntityLoader {

  def io(trans: Transactor[Task])(implicit strat: Strategy) = new EntityLoader[Task] {

    private def emptyTopicIO = Task.now(Map.empty[Topic, Seq[EntityMatch[TopicEntity]]])

    def topicEntities(topics: Seq[Topic]): Task[Map[Topic, Seq[EntityMatch[TopicEntity]]]] = {
      import Topic._

      val relNelO    = NonEmptyList.fromList(topics.flatMap(_.related.getOrElse(Nil).map(_.name)).distinct.toList)
      val topicsNelO = extractNames(topics)

      def query(namesO: Option[NonEmptyList[String]], score: Double): Task[Map[Topic, Seq[EntityMatch[TopicEntity]]]] = namesO.fold(emptyTopicIO) { names =>
        val entitiesQ = fr"SELECT id, name FROM topic_ontology WHERE " ++ Fragments.in(fr"name", names)

        entitiesQ.query[TopicEntity]
          .list
          .map(directMatch(topics, _, score))
          .transact(trans)
      }

      for {
        relEnt   <- query(relNelO, 0.5).async
        topicEnt <- query(topicsNelO, 1.0).async
      } yield topics.map { topic =>
        val main    = topicEnt.get(topic).getOrElse(Nil)
        val related = topic.related.getOrElse(Nil).flatMap { rel =>
          relEnt.get(rel).getOrElse(Nil)
        }

        topic -> (main ++: related)
      }(collection.breakOut)
    }

    private def emptyLangIO = Task.now(Map.empty[Language, Seq[EntityMatch[LanguageEntity]]])

    def languageEntities(languages: Seq[Language]): Task[Map[Language, Seq[EntityMatch[LanguageEntity]]]] = {
      import Language._

      extractNames(languages).fold(emptyLangIO) { names =>
        val entitiesQ = fr"SELECT id, name FROM language_ontology WHERE " ++ Fragments.in(fr"name", names)

        entitiesQ.query[LanguageEntity]
          .list
          .map(directMatch(languages, _, 1.0))
          .transact(trans)
      }
    }
  }

  private[loader] def extractNames[A: Labelled](as: Seq[A]): Option[NonEmptyList[String]] = {
    val labels: Set[String] = as.map(Labelled[A].label)(collection.breakOut)

    NonEmptyList.fromList(labels.toList)
  }

  // TODO implement proper matching strategy
  private[loader] def directMatch[A: Labelled, E <: Entity : Labelled](as: Seq[A], entities: Seq[E], score: Double): Map[A, Seq[EntityMatch[E]]] = {
    val byLabel = entities.groupBy(Labelled[E].label)

    val entitized: Map[A, Seq[EntityMatch[E]]] = as.flatMap { a =>
      byLabel.get(Labelled[A].label(a)).map(entity => a -> Seq(EntityMatch(entity.head, score)))
    }(collection.breakOut)

    entitized
  }

}
