package github.projects

import github.projects.data._
import github.projects.loader.{TopicEntityLoader, LanguageEntityLoader}
import github.projects.util.ParallelTraverse

import cats.{Monad, Applicative}
import cats.data.NonEmptyList
import cats.implicits._
import doobie.imports._

import scala.collection.mutable.Builder
import scala.language.higherKinds

object Entitize {

  type TopicMatches = EntityMatches[Topic, TopicEntity]

  def entitizeTopics[F[_]: Monad: ParallelTraverse: TopicEntityLoader](topics: List[Topic]): F[List[TopicMatches]] = {
    def pure(entities: List[TopicMatches]) = Monad[F].pure(entities)

    def q(names: NonEmptyList[String]) = fr"SELECT id, name FROM topic_ontology WHERE " ++ Fragments.in(fr"name", names)

    def run(topics: List[Topic], score: Double, remaining: Int, acc: Builder[TopicMatches, List[TopicMatches]]): F[List[TopicMatches]] =
      NonEmptyList.fromList(topics.map(_.name)).fold(pure(Nil)) { namesNel =>
        TopicEntityLoader[F].load(topics, q(namesNel).query[TopicEntity], score).flatMap { entitized =>
          acc ++= entitized

          if (remaining == 0)
            pure(acc.result())
          else
            ParallelTraverse[F]
              .parallelTraverse(topics) { topic =>
                topic.related.fold(pure(acc.result()))(related => run(related, score * 0.5, remaining - 1, acc))
              }
              .map(_.flatMap(identity)(collection.breakOut))
        }
      }

    run(topics, 1.0, 2, List.newBuilder)
  }

  type LanguageMatches = EntityMatches[Language, LanguageEntity]

  def entitizeLanguages[F[_]: Applicative: LanguageEntityLoader](languages: List[Language]): F[List[LanguageMatches]] = {
    def q(names: NonEmptyList[String]) = fr"SELECT id, name FROM language_ontology WHERE " ++ Fragments.in(fr"name", names)

    NonEmptyList.fromList(languages.map(_.name)).fold(Applicative[F].pure(List.empty[LanguageMatches])) { namesNel =>
      LanguageEntityLoader[F].load(languages, q(namesNel).query[LanguageEntity], 1.0)
    }
  }
}
