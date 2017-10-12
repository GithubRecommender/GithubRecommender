package github.projects.data

import io.circe._
import io.circe.generic.JsonCodec

@JsonCodec final case class TopicWrapper(topic: Topic)

final case class RepositoryInfo(id: String,
                                name: String,
                                description: String,
                                languages: Seq[Language],
                                topics: Seq[Topic])
object RepositoryInfo {

  implicit val repoDecoder = new Decoder[RepositoryInfo] {
    final def apply(c: HCursor): Decoder.Result[RepositoryInfo] = {
      val repoC = c.downField("data").downField("repository")

      for {
        id     <- repoC.downField("id").as[String]
        name   <- repoC.downField("name").as[String]
        desc   <- repoC.downField("description").as[String]
        langs  <- repoC.downField("languages").downField("nodes").as[Seq[Language]]
        topics <- repoC.downField("topics").downField("nodes").as[Seq[TopicWrapper]]
      } yield RepositoryInfo(id, name, desc, langs, topics.map(_.topic))
    }
  }
}
