package github.projects.data

import github.projects.util.Labelled

import io.circe.generic.JsonCodec

@JsonCodec final case class Topic(id: String, name: String, related: Option[List[Topic]])

final case class TopicEntity(id: Long, label: String) extends Entity

object Topic {

  implicit val topicLabelled = new Labelled[Topic] {
    @inline def label(a: Topic) = a.name
  }

  implicit val topicEInstacnes = new Labelled[TopicEntity] {
    @inline def label(a: TopicEntity) = a.label
  }
}
