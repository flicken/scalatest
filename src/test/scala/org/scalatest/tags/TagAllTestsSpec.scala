package org.scalatest.tags

import org.scalatest.{Tag, FunSpec}

class TagAllTestsSpec extends FunSpec {
   describe("A TagAllTests") {
     it("should not add tags when no tags are returned") {
       // TODO: Is this a bug in ScalaTest's use of tags map to not handle an empty Set?
       val spec = new FunSpec with TagAllTests {
         override def tagAllTestsWith = Set()
         it("should not tag this method") {}
       }
       assert(spec.tags.values.toList === List())
     }

     it("should tag a single test with a given tag") {
       val spec = new FunSpec with TagAllTests {
         override def tagAllTestsWith = Set("a tag")
         it("should tag this method") {}
       }
       assert(spec.tags.values.toList === List(Set("a tag")))
     }

     it("should tag a single test with multiple tags") {
       val spec = new FunSpec with TagAllTests {
         override def tagAllTestsWith = Set("a tag", "another tag")
         it("should tag this method") {}
       }
       assert(spec.tags.values.toList === List(Set("a tag", "another tag")))
     }

     it("should combine with tags with tags on method") {
       val spec = new FunSpec with TagAllTests {
         override def tagAllTestsWith = Set("a tag")
         it("should tag this method", Tag("tag arg in it")) {}
       }
       assert(spec.tags.values.toList === List(Set("a tag", "tag arg in it")))
     }

     it("should tag multiple tests with same tag") {
       val spec = new FunSpec with TagAllTests {
         override def tagAllTestsWith = Set("a tag", "another tag")
         it("should tag this method") {}
         it("should tag that method") {}
       }
       assert(spec.tags.values.toList === List(Set("a tag", "another tag"), Set("a tag", "another tag")))
     }

     it("should merge tags and tag everything") {
       val spec = new FunSpec with TagAllTests {
         override def tagAllTestsWith = Set("a tag", "another tag")
         it("should tag this method") {}
         it("should tag that method", Tag("tag arg in it")) {}
       }

       assert(spec.tags("should tag this method") === Set("a tag", "another tag"))
       assert(spec.tags("should tag that method") === Set("a tag", "another tag", "tag arg in it"))
     }
   }
 }
