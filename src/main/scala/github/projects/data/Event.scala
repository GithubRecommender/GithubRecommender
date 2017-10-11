package github.projects.data

import play.api.libs.json._

final case class Repository(id: Long, name: String, url: String)

object Repository {

  implicit val reads = Json.reads[Repository]
}

final case class Event(`type`: String, repo: Repository) {

  override def equals(obj: Any): Boolean = obj match {
    case Event(_type, Repository(id, _, _)) => _type == `type` && id == repo.id
    case _ => false
  }
}

object Event {

  implicit val reads = Json.reads[Event]
}
