package github.projects.loader

import org.http4s.client.blaze._
import org.specs2.mutable.Specification

final class GithubApiLoaderIoSpec extends Specification {

  val token  = System.getenv("GITHUB_API_TOKEN")
  val client = PooledHttp1Client()

  "load Github event archive" >> {
    "execute" >> {
      GithubApiLoader.io(token, client).load(QueryVariables("pheymann", "specdris")).map(_.map(_.name)).unsafeRun() === Right("specdris")
    }

    step {
      client.shutdownNow()
    }
  }
}
