package github.projects.loader

import org.http4s.client.blaze._

import org.specs2.mutable.Specification

final class ArchiveIOLoaderSpec extends Specification {

  val client = PooledHttp1Client()

  "load Github event archive" >> {
    "execute" >> {
      ArchiveIOLoader(client).load(2017, 1, 1, 12, Set("PullRequestEvent")).map(_.size).unsafeRun() === 766
    }

    step {
      client.shutdownNow()
    }
  }
}
