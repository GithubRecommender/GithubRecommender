package github.projects.loader

import github.projects.util.Slf4jLog
import github.projects.data._

import fs2.{Task, Chunk}
import fs2.interop.cats._
import org.http4s._
import org.http4s.client._
import io.circe.parser.decode

import scala.io.Source
import scala.language.higherKinds

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

trait GithubArchiveLoader[F[_]] extends {

  def load(year: Int, month: Int, day: Int, hour: Int, events: Set[String]): F[Iterator[Event]]
}

object GithubArchiveLoader {

  def io(client: Client) = new GithubArchiveLoader[Task] {

    private val log = Slf4jLog[Task]("github-archive-loader")

    def load(year: Int, month: Int, day: Int, hour: Int, events: Set[String]): Task[Iterator[Event]] = {
      val url = "http://data.githubarchive.org/%d-%02d-%02d-%02d.json.gz".format(year, month, day, hour)

      client
        .expect[Chunk[Byte]](Uri.unsafeFromString(url))
        .map { chunk =>
          Source
            .fromInputStream(new GZIPInputStream(new ByteArrayInputStream(chunk.toArray))).getLines
            .flatMap { raw =>
              decode[Event](raw) match {
                case Right(event) => if (events(event.`type`)) Some(event) else None
                case Left(error) =>
                  log.error(s"failed to parse event from archive: $url\n$error")
                  None
              }
            }            
        }
    }
  }
}
