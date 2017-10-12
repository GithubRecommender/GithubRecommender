package github.projects.loader

import github.projects.data._

import fs2.{Task, Stream, Chunk}
import org.http4s._
import org.http4s.client._
import play.api.libs.json._

import scala.language.higherKinds

final case class QueryVariables(owner: String, name: String)

trait GithubApiLoader[F[_]] {

  def load(variables: QueryVariables): F[Either[Throwable, RepositoryInfo]]
}

object ApiIOLoader {

  private val LangFirst  = 10
  private val TopicFirst = 50

  private val Query = 
     s"""fragment topicInfo on Topic {
        |  topicId: id
        |  topicName: name
        |}
        |
        |query RepositoryInfo($$name: String!, $$owner: String!) {
        |  repository(name: $$name, owner: $$owner) {
        |    id
        |    name
        |    description
        |    isFork
        |    isPrivate
        |    isMirror
        |    isArchived
        |    isLocked
        |    languages(first: $LangFirst, orderBy: {field: SIZE, direction: DESC}) {
        |      nodes {
        |        languageId: id
        |        languageName: name
        |      }
        |    }
        |
        |    topics: repositoryTopics(first: $TopicFirst) {
        |      nodes {
        |        topic {
        |          ...topicInfo
        |
        |          relatedTopics {
        |            ...topicInfo
        |          }
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin

  final case class QueryRequest(variables: QueryVariables, query: String = Query)

  implicit val varWrites = Json.writes[QueryVariables]
  implicit val reqWrites = Json.writes[QueryRequest]

  final case class ParseException(error: String)
      extends Exception(s"parser error = $error")

  final case class GithubApiException(owner: String, project: String, cause: Throwable)
      extends Exception(s"Github api query for $owner/$project failed", cause)

  def apply(token: String, client: Client) = new GithubApiLoader[Task] {

    private val uri = Uri.unsafeFromString("https://api.github.com/graphql")

    private def extract(json: JsValue): Option[JsValue] = (json \ "data" \ "repository").toOption

    def load(variables: QueryVariables): Task[Either[Throwable, RepositoryInfo]] = {
      val request = QueryRequest(variables)
      val body    = Stream.chunk[Task, Byte](Chunk.bytes(Json.toJson(request).toString.getBytes))

      client
        .expect[Chunk[Byte]](Request(Method.POST, uri).withBodyStream(body).withHeaders(Headers(Header("Authorization", s"bearer $token"))))
        .map { chunk =>
          extract(Json.parse(chunk.toArray)) match {
            case Some(repoJson) =>
              Json.fromJson[RepositoryInfo](repoJson) match {
                case JsSuccess(info, _) => info
                case JsError(errors)    => throw ParseException(errors.mkString("\n"))
              }

            case None => throw ParseException("no data: { repository: {...} } in response")
          }
        }
        .attempt
        .map {
          case r@Right(_)  => r
          case Left(cause) =>
            cause.printStackTrace()
            Left(GithubApiException(variables.owner, variables.name, cause))
        }
    }
  }
}
