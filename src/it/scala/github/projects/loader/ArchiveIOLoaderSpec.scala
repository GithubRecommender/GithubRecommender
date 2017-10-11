package github.projects.loader

import org.http4s.client.blaze._

import org.specs2.mutable.Specification

final class ArchiveIOLoaderSpec extends Specification {

  val client = PooledHttp1Client()

  "load Github event archive" >> {
    "execute" >> {
      ArchiveIOLoader.init(client).load(2017, 1, 1, 12, Set("PullRequestEvent")).map(_.nonEmpty).unsafeRun() should beTrue
    }

    step {
      client.shutdownNow()
    }
  }
}
