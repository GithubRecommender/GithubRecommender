package github.projects

import github.projects.util.ParallelTraverse
import github.projects.data._
import github.projects.reader._

import cats.Id

import org.specs2.mutable.Specification

final class EntitizeSpec extends Specification {

  import Entitize._

  def isRelatedId(id: Long): Boolean = id > 10
  final val WithRelatedIds = 10

  "entitize" >> {
    "topics" >> {
      implicit val ev = testTopic {
        case (Topic(idStr, name, relatedO), score) => Right(List(EntityMatch(TopicEntity(idStr.toLong, name), score)))
      }

      implicit val idPar = new ParallelTraverse[Id] {
        def parallelTraverse[A, B](s: Seq[A])(f: A => Id[B]): Id[Vector[B]] = s.map(f)(collection.breakOut)
      }

      entitizeTopics[Id](Nil) === Nil
      entitizeTopics[Id](List(Topic("0", "a", None))) === List(
        Right(List(EntityMatch(TopicEntity(0L, "a"), 1.0)))
      )
      entitizeTopics[Id](List(Topic("0", "a", Some(List(Topic("1", "b", None)))))) === List(
        Right(List(EntityMatch(TopicEntity(0L, "a"), 1.0))),
        Right(List(EntityMatch(TopicEntity(1L, "b"), 0.5)))
      )
      entitizeTopics[Id](List(Topic("0", "a", Some(List(Topic("1", "b", Some(List(Topic("2", "c", None))))))))) === List(
        Right(List(EntityMatch(TopicEntity(0L, "a"), 1.0))),
        Right(List(EntityMatch(TopicEntity(1L, "b"), 0.5))),
        Right(List(EntityMatch(TopicEntity(2L, "c"), 0.25)))
      )
    }

    "languages" >> {
      implicit val t = testLanguage {
        case (Language(idStr, name), score) => Right(List(EntityMatch(LanguageEntity(idStr.toLong, name), score)))
      }

      entitizeLanguages[Id](Nil) === Nil
      entitizeLanguages[Id](List(Language("0", "a"))) === List(
        Right(List(EntityMatch(LanguageEntity(0L, "a"), 1.0)))
      )
    }
  }

  def testTopic(f: (Topic, Double) => TopicMatches) = new TopicEntityReader[Id] {
    def read(as: List[Topic], score: Double = 1.0): Id[List[TopicMatches]] = as.map(f(_, score))
  }

  def testLanguage(f: (Language, Double) => LanguageMatches) = new LanguageEntityReader[Id] {
    def read(as: List[Language], score: Double = 1.0): Id[List[LanguageMatches]] = as.map(f(_, score))
  }
}
