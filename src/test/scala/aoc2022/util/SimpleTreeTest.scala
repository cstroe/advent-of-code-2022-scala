package aoc2022.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimpleTreeTest extends AnyFlatSpec with Matchers {
  "A Simple tree" should "insert nodes" in {
    val tree = SimpleTree[String, List[String]]()
    // tree.insert("" :: Nil, List("a", "b", "c"))
    // tree.get("" :: Nil) shouldBe List("a", "b", "c")
  }
}
