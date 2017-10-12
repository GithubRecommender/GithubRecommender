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

      def toMatch[E <: Entity](entity: E) = Seq(EntityMatch(entity, 0.0))

      EntityLoader.directMatch[Language, LanguageEntity](Nil, Nil, 0.0) === Map.empty
      EntityLoader.directMatch(Seq(de), Seq(deE), 0.0) === Map(de -> toMatch(deE))
      EntityLoader.directMatch(Seq(de, en), Seq(deE, enE), 0.0) === Map(de -> toMatch(deE), en -> toMatch(enE))
      EntityLoader.directMatch(Seq(de, en), Seq(deE), 0.0) === Map(de -> toMatch(deE))
      EntityLoader.directMatch(Seq(de, en), Nil, 0.0) === Map.empty
    }
  }
}
