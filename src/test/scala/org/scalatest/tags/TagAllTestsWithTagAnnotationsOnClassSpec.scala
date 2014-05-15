package org.scalatest.tags

import org.scalatest.{Tag, FunSpec}
import org.scalatest.StrongAsSuperman

class TagAllTestsWithTagAnnotationsOnClassSpec extends FunSpec {
  describe("A TagAllTestsWithTagAnnotationsOnClass") {
    it("should not add tags when no annotations are present") {
      val spec = new FunSpec with TagAllTestsWithTagAnnotationsOnClass {
         it("should not tag this method") { assert(1 === 1) }
      }
      assert(spec.tags.values.toList === List())
    }

    it("should tag a single test with annotation on super class") {
      val spec = new BaseAnnotatedSpec {
         it("should tag this method") { assert(1 === 1) }
      }
      assert(spec.tags.values.toList === List(Set("org.scalatest.StrongAsSuperman")))
    }

    it("should combine tags on a single test") {
      val spec = new BaseAnnotatedSpec {
         it("should tag this method with another tag", Tag("another tag")) { assert(1 === 1) }
      }
      assert(spec.tags.values.toList === List(Set("org.scalatest.StrongAsSuperman", "another tag")))
    }

    it("should handle multiple cases at once") {
      val spec = new BaseAnnotatedSpec {
        it("should tag this method") { assert(1 === 1) }
        it("should tag this method with another tag", Tag("another tag")) { assert(1 === 1) }
      }

      assert(spec.tags("should tag this method") === Set("org.scalatest.StrongAsSuperman"))
      assert(spec.tags("should tag this method with another tag") === Set("org.scalatest.StrongAsSuperman", "another tag"))
    }
  }

  @StrongAsSuperman
  private class BaseAnnotatedSpec extends FunSpec with TagAllTestsWithTagAnnotationsOnClass {}
}
