package github.projects.reader

import github.projects.data._

import org.specs2.mutable.Specification

final class EntityReaderSpec extends Specification {

  "EntityReader helper" >> {
    "entitize by relating entities to ontology concepts" >> {
      import Language._

      val de = Language("0", "de")
      val en = Language("1", "en")

      val deE = LanguageEntity(0L, de.name)
      val enE = LanguageEntity(1L, en.name)

      def toMatch[E <: Entity](entity: E) = List(EntityMatch(entity, 0.0))

      EntityReader.directMatch[Language, LanguageEntity](Nil, Nil, 0.0) === List.empty
      EntityReader.directMatch(List(de), List(deE), 0.0) === List(Right(toMatch(deE)))
      EntityReader.directMatch(List(de, en), List(deE, enE), 0.0) === List(Right(toMatch(deE)), Right(toMatch(enE)))
      EntityReader.directMatch(List(de, en), List(deE), 0.0) === List(Right(toMatch(deE)), Left(en))
      EntityReader.directMatch(List(de, en), Nil, 0.0) === List(Left(de), Left(en))
    }
  }
}
