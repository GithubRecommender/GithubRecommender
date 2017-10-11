package github.projects.writer

import github.projects.data.RepositoryInfo

import scala.language.higherKinds

trait RepositoryInfoWriter[F[_]] {

  def write(info: RepositoryInfo): F[Unit]
}
