package github.projects

import github.projects.data.Event

object Deduplicate {

  def apply(events: Seq[Event]): Set[Event] = events.toSet
}
