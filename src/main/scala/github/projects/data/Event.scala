package github.projects.data

import io.circe.generic.JsonCodec

@JsonCodec final case class Repository(id: Long, name: String, url: String)

@JsonCodec final case class Event(`type`: String, repo: Repository)

