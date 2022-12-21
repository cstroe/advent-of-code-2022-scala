package aoc2022

import aoc2022.Day13.{LeftOutOfItems, RightOutOfItems}
import org.junit.Test
import org.scalatest.matchers.should.Matchers

class Day13Test extends Matchers {

  @Test
  def correctlyDoTheExampleInputs(): Unit = {
    Day13.inOrder("[1,1,3,1,1]", "[1,1,5,1,1]") shouldBe true
    Day13.inOrder("[[1],[2,3,4]]", "[[1],4]") shouldBe true
    Day13.inOrder("[9]", "[[8,7,6]]") shouldBe false
    Day13.inOrder("[[4,4],4,4]", "[[4,4],4,4,4]") shouldBe true
    Day13.compare("[[4,4],4,4]", "[[4,4],4,4,4]") shouldBe LeftOutOfItems

    Day13.inOrder("[7,7,7,7]", "[7,7,7]") shouldBe false
    Day13.compare("[7,7,7,7]", "[7,7,7]") shouldBe RightOutOfItems

    Day13.inOrder("[[[]]]", "[[]]") shouldBe false
    Day13.compare("[[[]]]", "[[]]") shouldBe RightOutOfItems

    Day13.inOrder("[]", "[3]") shouldBe true
    Day13.compare("[]", "[3]") shouldBe LeftOutOfItems
  }

  @Test
  def itShouldCompareEqualListsOfDifferentSizes(): Unit = {
    Day13.inOrder("[7,7,7,7]", "[7,7,7]") shouldBe false
    Day13.inOrder("[7,7,7]", "[7,7,7]") shouldBe true
    Day13.inOrder("[7,7,7]", "[7,7,7,7]") shouldBe true
  }

  @Test
  def itShouldCompareWithLists(): Unit = {
    Day13.inOrder("[2,3,4]", "[4]") shouldBe true
  }
}
