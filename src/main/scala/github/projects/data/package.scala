package github.projects

package object data {

  trait Entity

  final case class EntityMatch[E <: Entity](entity: E, score: Double)

  type EntityMatches[A, E <: Entity] = Either[A, List[EntityMatch[E]]]
}
