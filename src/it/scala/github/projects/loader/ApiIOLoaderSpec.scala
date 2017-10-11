package github.projects.loader

import org.http4s.client.blaze._

import org.specs2.mutable.Specification

final class ApiIOLoaderSpec extends Specification {

  val token  = "e0f0c99533d347b3893b359e6e8242601a64b405"
  val client = PooledHttp1Client()

  "load Github event archive" >> {
    "execute" >> {
      ApiIOLoader.init(token, client).load("pheymann", "specdris").map(_.name).unsafeRun() === "specdris"
    }

    step {
      client.shutdownNow()
    }
  }
}
