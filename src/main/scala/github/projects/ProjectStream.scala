package github.projects

import github.projects.data._
import github.projects.reader._
import github.projects.writer._
import github.projects.util.ParallelTraverse

import cats.Monad
import cats.implicits._
import fs2._
import fs2.util.Async

import scala.language.higherKinds

import java.time.LocalDateTime

object ProjectStream {

  type GAr[F[_]]  = GithubArchiveReader[F]
  type GApi[F[_]] = GithubApiReader[F]
  type PT[F[_]]   = ParallelTraverse[F]
  type TR[F[_]]   = TopicEntityReader[F]
  type TE[F[_]]   = TopicToEntity[F]
  type LR[F[_]]   = LanguageEntityReader[F]
  type LE[F[_]]   = LanguageToEntity[F]
  type RW[F[_]]   = RepositoryWriter[F]

  private def withConsumers[F[_]: Monad: Async: GAr: GApi: PT: TR: TE: LR: LE: RW](s: Stream[F, Event], index: Int, total: Int): Stream[F, Event] = {
    if (index == total) 
      s
    else {
      val _s = s.observe[F, Event](_.filter(_.repo.id % total == index).to(_.map { event =>
        for {
          repo          <- GithubApiReader[F].read(QueryVariables("", ""))
          topicMatchesE <- Entitize.entitizeTopics[F](repo.topics)
          langMatchesE   <- Entitize.entitizeLanguages[F](repo.languages)

          newTopicMatches <- {
            ParallelTraverse[F]
              .parallelTraverse(topicMatchesE.collect {
                case Left(topic) => topic
              })(TopicToEntity[F].write)
              .map { es => 
                val result: List[EntityMatch[TopicEntity]] = es.map(e => EntityMatch(e, 1.0))(collection.breakOut)
                result
              }
          }

          newLangMatches <- {
            ParallelTraverse[F]
              .parallelTraverse(langMatchesE.collect {
                case Left(language) => language
              })(LanguageToEntity[F].write)
              .map { es => 
                val result: List[EntityMatch[LanguageEntity]] = es.map(e => EntityMatch(e, 1.0))(collection.breakOut)
                result
              }
          }

          topicMatches = topicMatchesE.collect {
            case Right(_match) => _match
          }.flatten
          langMatches = langMatchesE.collect {
            case Right(_match) => _match
          }.flatten

          _ <- RepositoryWriter[F].write(repo, newTopicMatches ++ topicMatches, newLangMatches ++ langMatches)
        } yield ()
      }.drain))

      withConsumers(_s, index + 1, total)
    }
  }

  def apply[F[_]: Monad: Async: GAr: GApi: PT: TR: TE: LR: LE: RW](archive: LocalDateTime, parallelism: Int): F[Unit] = {
    for {
      events <- GithubArchiveReader[F].read(archive)
    } yield {
      val s = Stream
        .unfold(events)(e => if (e.hasNext) Some((e.next,e)) else None)
        .filter(_.`type` == "test")

      withConsumers[F](s, 0, parallelism)
    }

    ???
  }
}
