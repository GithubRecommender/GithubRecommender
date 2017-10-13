package github.projects.util

import fs2.{Task, Strategy}

import scala.language.higherKinds

trait ParallelTraverse[F[_]] {

  def parallelTraverse[A,B](s: Seq[A])(f: A => F[B]): F[Vector[B]]
}

object ParallelTraverse {

  @inline def apply[F[_]](implicit pt: ParallelTraverse[F]): ParallelTraverse[F] = pt

  def taskInstance(implicit strat: Strategy) = new ParallelTraverse[Task] {
    @inline def parallelTraverse[A,B](s: Seq[A])(f: A => Task[B]): Task[Vector[B]] = Task.parallelTraverse(s)(f)
  }
}
