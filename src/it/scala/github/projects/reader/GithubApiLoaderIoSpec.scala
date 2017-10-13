package github.projects.reader

import org.http4s.client.blaze._
import org.specs2.mutable.Specification

final class GithubApiReaderIoSpec extends Specification {

  val token  = System.getenv("GITHUB_API_TOKEN")
  val client = PooledHttp1Client()

  "load Github event archive" >> {
    "execute" >> {
      GithubApiReader.io(token, client).read(QueryVariables("pheymann", "specdris")).map(_.map(_.name)).unsafeRun() === Right("specdris")
    }

    step {
      client.shutdownNow()
    }
  }
}
