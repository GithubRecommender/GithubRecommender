package github.projects.reader

import org.http4s.client.blaze._
import org.specs2.mutable.Specification

import java.time._

final class GithubArchiveReaderIoSpec extends Specification {

  val client = PooledHttp1Client()

  "load Github event archive" >> {
    "execute" >> {
      GithubArchiveReader.io(client).read(LocalDateTime.of(2017, 1, 1, 12, 0)).map(_.size).unsafeRun() === 18241
    }

    step {
      client.shutdownNow()
    }
  }
}
