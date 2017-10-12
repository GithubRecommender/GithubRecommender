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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

trait GithubArchiveLoader[F[_]] extends {

  def load(archiveDate: LocalDateTime): F[Iterator[Event]]
}

object GithubArchiveLoader {

  def io(client: Client) = new GithubArchiveLoader[Task] {

    private val log = Slf4jLog[Task]("github-archive-loader")

    def load(archiveDate: LocalDateTime): Task[Iterator[Event]] = {
      val url = s"http://data.githubarchive.org/${archiveDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd-HH"))}.json.gz"

      client
        .expect[Chunk[Byte]](Uri.unsafeFromString(url))
        .map { chunk =>
          Source
            .fromInputStream(new GZIPInputStream(new ByteArrayInputStream(chunk.toArray))).getLines
            .flatMap { raw =>
              decode[Event](raw) match {
                case Right(event) => Some(event)
                case Left(error)  =>
                  log.error(s"failed to parse event from archive: $url\n$error")
                  None
              }
            }            
        }
    }
  }
}
