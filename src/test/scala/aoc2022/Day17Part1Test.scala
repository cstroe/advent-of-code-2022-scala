package aoc2022

import aoc2022.Day17Part1.{CrossRock, FallingRock, FlatRock, Point}
import org.junit.Test
import org.scalatest.matchers.should.Matchers

class Day17Part1Test extends Matchers {

  @Test
  def correctlyCreatePoints(): Unit = {
    FallingRock(FlatRock(), Point(0, 0)).toPoints() shouldBe Set(
      Point(0,0), Point(1,0), Point(2,0), Point(3,0)
    )
    FallingRock(CrossRock(), Point(0, 0)).toPoints() shouldBe Set(
      Point(1, 0), Point(0, -1), Point(1, -1), Point(2, -1), Point(1,-2)
    )
  }

  @Test
  def correctlyFindIntersectingRocks(): Unit = {
    val bottomRock = FallingRock(FlatRock(), Point(0, 0))
    val topRock = FallingRock(FlatRock(), Point(0, 1))
  }
}
