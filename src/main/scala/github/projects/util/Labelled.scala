package github.projects.util

trait Labelled[A] {

  def label(a: A): String
}

object Labelled {

  @inline def apply[A](implicit l: Labelled[A]): Labelled[A] = l
}
