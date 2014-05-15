package org.scalatest.tags

import org.scalatest.{TagAnnotation, Suite}

trait TagAllTestsWithTagAnnotationsOnClass extends TagAllTests { self: Suite =>
  override def tagAllTestsWith = {
    val tagAnnotations = self.getClass.getAnnotations.filter(_.annotationType().getAnnotation(classOf[TagAnnotation]) != null)
    tagAnnotations.map(_.annotationType().getCanonicalName).toSet
  }
}
