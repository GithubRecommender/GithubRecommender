package github.projects.util

import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.language.higherKinds

trait Log[F[_]] {

  def error(msg: => String): Unit
  def error(msg: => String, cause: Throwable): Unit
}

object Log {

  @inline def apply[F[_]](implicit l: Log[F]): Log[F] = l
}

object Slf4jLog {

  def init[A](name: String) = new Log[Future] {

    private val log = LoggerFactory.getLogger(name)

    def error(msg: => String): Unit =
      log.error(msg)

    def error(msg: => String, cause: Throwable): Unit =
      log.error(msg, cause)
  }
}
