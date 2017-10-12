package github.projects.data

import github.projects.util.Labelled

import io.circe.generic.JsonCodec

@JsonCodec final case class Language(id: String, name: String)

final case class LanguageEntity(id: Long, label: String) extends Entity

object Language {

  implicit val langLabelled = new Labelled[Language] {
    @inline def label(a: Language) = a.name
  }

  implicit val langELabelled = new Labelled[LanguageEntity] {
    @inline def label(a: LanguageEntity) = a.label
  }
}
