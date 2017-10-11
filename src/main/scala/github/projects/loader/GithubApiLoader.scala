package github.projects.loader

import github.projects.util.Slf4jLog
import github.projects.data._

import fs2.{Task, Stream, Chunk}
import org.http4s._
import org.http4s.client._
import play.api.libs.json._

import scala.language.higherKinds

trait GithubApiLoader[F[_]] {

  def load(owner: String, name: String): F[RepositoryInfo]
}

object ApiIOLoader {

  private val TopicFragment =
    """fragment topicData on Topic {
      |  id
      |  name
      |}
    """.stripMargin

  private val LangFirst  = 10
  private val TopicFirst = 50

  private def query(owner: String, name: String) = TopicFragment +
    s"""query {
       |    repository(name: "$name", owner: "$owner") {
       |      id
       |      name
       |      description
       |
       |      languages(first: $LangFirst, orderBy: {field: SIZE, direction: DESC}) {
       |        nodes {
       |          id
       |          name
       |        }
       |      }
       |
       |      repositoryTopics(first: $TopicFirst) {
       |        nodes {
       |          topic {
       |            ...topicData 
       |   
       |            relatedTopics {
       |              ...topicData
       |            }
       |          }
       |        }
       |      }
       |    }
       |}
    """.stripMargin

  final case class HttpApiParseException(query: String, error: String)
      extends Exception(s"Github api: $query\nparser error = $error")

  def init(token: String, client: Client) = new GithubApiLoader[Task] {

    private val log = Slf4jLog.init("github-archive-loader")
    private val uri = Uri.unsafeFromString("https://api.github.com/graphql")

    def load(owner: String, name: String): Task[RepositoryInfo] = {
      val q    = s""""query":"${query(owner, name)}""""
      val body = Stream.chunk[Task, Byte](Chunk.bytes(q.getBytes))

      val responseT = client
        .expect[Chunk[Byte]](Request(Method.POST, uri).withBodyStream(body).withHeaders(Headers(Header("Authorization", s"bearer $token"))))
        .map { chunk => 
          Json.fromJson[RepositoryInfo](Json.parse(chunk.toArray)) match {
            case JsSuccess(info, _) => info
            case JsError(errors)    => throw HttpApiParseException(q, errors.mkString("\n"))
          }
        }

      responseT.unsafeRunAsync {
        case Left(cause) => log.error(s"failed Github query = $q", cause)
        case _           => ()
      }
      responseT
    }
  }
}
