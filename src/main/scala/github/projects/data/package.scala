package github.projects

package object data {

  trait Entity

  final case class EntityMatch[E <: Entity](entity: E, score: Double)
}
