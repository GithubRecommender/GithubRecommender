package github.projects.loader

import github.projects.util.Labelled
import github.projects.data._

import doobie.imports._
import doobie.util.query.Query0
import doobie.util.transactor.Transactor
import fs2.{Task, Strategy}
import fs2.interop.cats._

import scala.language.higherKinds

trait EntityLoader[F[_]] {

  def load[A: Labelled, E <: Entity: Labelled](as: List[A], query: Query0[E], score: Double = 1.0): F[List[EntityMatches[A, E]]]
}

object EntityLoader {

  @inline def apply[F[_]](implicit l: EntityLoader[F]): EntityLoader[F] = l

  def io(trans: Transactor[Task])(implicit strat: Strategy) = new EntityLoader[Task] {

    def load[A: Labelled, E <: Entity: Labelled](as: List[A], query: Query0[E], score: Double = 1.0): Task[List[EntityMatches[A, E]]] = 
      query
        .list
        .map(directMatch[A, E](as, _, score))
        .transact(trans)
  }

  // TODO implement proper matching strategy
  private[loader] def directMatch[A: Labelled, E <: Entity : Labelled](as: List[A], entities: List[E], score: Double): List[EntityMatches[A, E]] = {
    val byLabel = entities.groupBy(Labelled[E].label)

    as.map { a =>
      byLabel.get(Labelled[A].label(a)).fold[EntityMatches[A, E]](Left(a))(es => Right(es.map(entity => EntityMatch(entity, score))))
    }
  }

}
