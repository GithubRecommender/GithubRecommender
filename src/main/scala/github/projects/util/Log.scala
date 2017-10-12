package github.projects.util

import cats.Applicative
import org.slf4j.LoggerFactory

import scala.language.higherKinds

trait Log[F[_]] {

  def error(msg: => String): F[Unit]
  def error(msg: => String, cause: Throwable): F[Unit]
}

object Log {

  @inline def apply[F[_]](implicit l: Log[F]): Log[F] = l
}

object Slf4jLog {

  def apply[F[_]: Applicative](name: String) = new Log[F] {

    private val log = LoggerFactory.getLogger(name)

    def error(msg: => String): F[Unit] =
      Applicative[F].pure(log.error(msg))

    def error(msg: => String, cause: Throwable): F[Unit] =
      Applicative[F].pure(log.error(msg, cause))
  }
}
