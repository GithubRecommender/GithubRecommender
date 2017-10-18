package github.projects.reader

import github.projects.data._

import io.circe.syntax._
import io.circe.generic.JsonCodec
import fs2.Task
import org.http4s._
import org.http4s.client._
import org.http4s.circe._

import scala.language.higherKinds

@JsonCodec final case class QueryVariables(owner: String, name: String)
@JsonCodec final case class QueryRequest(variables: QueryVariables, query: String)

trait GithubApiReader[F[_]] {

  def read(variables: QueryVariables): F[RepositoryInfo]
}

object GithubApiReader {

  import RepositoryInfo._

  @inline def apply[F[_]](implicit g: GithubApiReader[F]): GithubApiReader[F] = g

  final case class GithubApiException(owner: String, project: String, cause: Throwable)
      extends Exception(s"Github api query for $owner/$project failed", cause)

  def io(token: String, client: Client) = new GithubApiReader[Task] {

    private val LangFirst  = 10
    private val TopicFirst = 50

    private val Query =
      s"""fragment topicInfo on Topic {
          |  id
          |  name
          |}
          |
          |query RepositoryInfo($$name: String!, $$owner: String!) {
          |  repository(name: $$name, owner: $$owner) {
          |    id
          |    owner {
          |      login
          |    }
          |    name
          |    description
          |    isFork
          |    isPrivate
          |    isMirror
          |    isArchived
          |    isLocked
          |    languages(first: $LangFirst, orderBy: {field: SIZE, direction: DESC}) {
          |      nodes {
          |        id
          |        name
          |      }
          |    }
          |
          |    topics: repositoryTopics(first: $TopicFirst) {
          |      nodes {
          |        topic {
          |          ...topicInfo
          |
          |          related: relatedTopics {
          |            ...topicInfo
          |          }
          |        }
          |      }
          |    }
          |  }
          |}""".stripMargin

    private val uri = Uri.unsafeFromString("https://api.github.com/graphql")

    def read(variables: QueryVariables): Task[RepositoryInfo] = {
      val request = Request(Method.POST, uri)
        .withHeaders(Headers(Header("Authorization", s"bearer $token")))
        .withBody(QueryRequest(variables, Query).asJson)

      client
        .expect[RepositoryInfo](request)(jsonOf)
        .attempt
        .map {
          case Right(v)    => v
          case Left(cause) => throw GithubApiException(variables.owner, variables.name, cause)
        }
    }
  }
}
