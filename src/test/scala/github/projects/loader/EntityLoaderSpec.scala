package github.projects.loader

import github.projects.data._

import org.specs2.mutable.Specification

final class EntityLoaderSpec extends Specification {

  "EntityLoader helper" >> {
    "entitize by relating entities to ontology concepts" >> {
      import Language._

      val de = Language("0", "de")
      val en = Language("1", "en")

      val deE = LanguageEntity(0L, de.name)
      val enE = LanguageEntity(1L, en.name)

      def toMatch[E <: Entity](entity: E) = List(EntityMatch(entity, 0.0))

      EntityLoader.directMatch[Language, LanguageEntity](Nil, Nil, 0.0) === List.empty
      EntityLoader.directMatch(List(de), List(deE), 0.0) === List(Right(toMatch(deE)))
      EntityLoader.directMatch(List(de, en), List(deE, enE), 0.0) === List(Right(toMatch(deE)), Right(toMatch(enE)))
      EntityLoader.directMatch(List(de, en), List(deE), 0.0) === List(Right(toMatch(deE)), Left(en))
      EntityLoader.directMatch(List(de, en), Nil, 0.0) === List(Left(de), Left(en))
    }
  }
}
