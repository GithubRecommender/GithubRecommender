package github.projects.data

import play.api.libs.json._

final case class Language(id: String, name: String)

object Language {

  implicit val reads = Json.reads[Language]
}

final case class Topic(id: String, name: String, related: Seq[Topic])

object Topic {

  implicit lazy val reads: Reads[Topic] = Reads[Topic] { json =>
    for {
      id      <- (json \ "id").validate[String]
      name    <- (json \ "name").validate[String]
      related <- (json \ "relatedTopics").validateOpt[Seq[Topic]](Reads.seq[Topic](reads)).map(_.getOrElse(Nil))
    } yield Topic(id, name, related)
  }
}

final case class RepositoryInfo(id: String,
                                name: String,
                                description: String,
                                languages: Seq[Language],
                                topics: Seq[Topic])
object RepositoryInfo {

  implicit val reads = Reads[RepositoryInfo] { json =>
    for {
      id   <- (json \ "id").validate[String]
      name <- (json \ "name").validate[String]
      desc <- (json \ "description").validate[String]
      langs  <- (json \ "languages" \ "nodes").validate[Seq[Language]]
      topics <- (json \ "repositoryTopics" \ "nodes").validate[Seq[Topic]]
    } yield RepositoryInfo(id, name, desc, langs, topics)
  }
}
