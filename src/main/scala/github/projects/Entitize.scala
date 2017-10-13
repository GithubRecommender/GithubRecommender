package github.projects

import github.projects.data._
import github.projects.reader.{TopicEntityReader, LanguageEntityReader}
import github.projects.util.ParallelTraverse

import cats.Monad
import cats.implicits._

import scala.collection.mutable.Builder
import scala.language.higherKinds

object Entitize {

  type TopicMatches = EntityMatches[Topic, TopicEntity]

  def entitizeTopics[F[_]: Monad: ParallelTraverse: TopicEntityReader](topics: List[Topic]): F[List[TopicMatches]] = {
    def pure(entities: List[TopicMatches]) = Monad[F].pure(entities)

    def run(topics: List[Topic], score: Double, remaining: Int, acc: Builder[TopicMatches, List[TopicMatches]]): F[List[TopicMatches]] =
      TopicEntityReader[F].read(topics, score).flatMap { entitized =>
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

    run(topics, 1.0, 2, List.newBuilder)
  }

  type LanguageMatches = EntityMatches[Language, LanguageEntity]

  def entitizeLanguages[F[_]: LanguageEntityReader](languages: List[Language]): F[List[LanguageMatches]] =
    LanguageEntityReader[F].read(languages, 1.0)
}
