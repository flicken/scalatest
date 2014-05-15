package org.scalatest.tags

import org.scalatest.{Suite, SuiteMixin}

trait TagAllTests extends SuiteMixin { self: Suite =>
  def tagAllTestsWith: Set[String]

  private def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] =
    (for (m <- ms; kv <- m) yield kv).foldLeft(Map.empty[A, B]) {
      (a, kv) => a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
    }

  abstract override def tags: Map[String, Set[String]] = {
    val tags = tagAllTestsWith
    if (tags.isEmpty) { super.tags } else mergeMap(List(super.tags, testNames.map(_ -> tags).toMap))(_ ++ _)
  }
}

