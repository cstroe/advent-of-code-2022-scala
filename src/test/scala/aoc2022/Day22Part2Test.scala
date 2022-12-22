package aoc2022

import aoc2022.Day22Part2.{Direction, Down, Left, Right, Up, getNextPosition}
import org.junit.Test
import org.scalatest.matchers.should.Matchers

class Day22Part2Test extends Matchers {
  @Test
  def shouldWalkOutOfCube0(): Unit = {
    getNextPosition(0, 49, 24, Down) shouldBe (2, 0, 24, Down)
    getNextPosition(0, 30, 49, Right) shouldBe (1, 30, 0, Right)
    getNextPosition(0, 0, 24, Up) shouldBe (5, 24, 0, Right)
    getNextPosition(0, 30, 0, Left) shouldBe (3, 19, 0, Right)

    getNextPosition(1, 49, 24, Down) shouldBe(2, 24, 49, Left)
    getNextPosition(1, 30, 49, Right) shouldBe(4, 19, 49, Left)
    getNextPosition(1, 0, 24, Up) shouldBe(5, 49, 24, Up)
    getNextPosition(1, 30, 0, Left) shouldBe(0, 30, 49, Left)

    getNextPosition(2, 49, 24, Down) shouldBe (4, 0, 24, Down)
    getNextPosition(4, 49, 24, Down) shouldBe (5, 24, 49, Left)
    getNextPosition(3, 49, 24, Down) shouldBe (5, 0, 24, Down)
    getNextPosition(5, 49, 24, Down) shouldBe (1, 0, 24, Down)
  }

  @Test
  def walkAroundTheWorldUp(): Unit = {
    Seq(Up, Down, Left, Right).foreach { testDir =>
      val startAt: (Int, Int, Int, Direction) = (0, 35, 15, testDir)
      (0 until 200).foldLeft(startAt) { case ((cubeNum, row, col, direction), _) =>
        getNextPosition(cubeNum, row, col, direction)
      } shouldBe startAt

      val startAt1: (Int, Int, Int, Direction) = (1, 36, 10, testDir)
      (0 until 200).foldLeft(startAt1) { case ((cubeNum, row, col, direction), _) =>
        getNextPosition(cubeNum, row, col, direction)
      } shouldBe startAt1

      val startAt2: (Int, Int, Int, Direction) = (2, 46, 20, testDir)
      (0 until 200).foldLeft(startAt2) { case ((cubeNum, row, col, direction), _) =>
        getNextPosition(cubeNum, row, col, direction)
      } shouldBe startAt2

      val startAt3: (Int, Int, Int, Direction) = (3, 16, 40, testDir)
      (0 until 200).foldLeft(startAt3) { case ((cubeNum, row, col, direction), _) =>
        getNextPosition(cubeNum, row, col, direction)
      } shouldBe startAt3

      val startAt4: (Int, Int, Int, Direction) = (4, 6, 2, testDir)
      (0 until 200).foldLeft(startAt4) { case ((cubeNum, row, col, direction), _) =>
        getNextPosition(cubeNum, row, col, direction)
      } shouldBe startAt4

      val startAt5: (Int, Int, Int, Direction) = (5, 20, 49, testDir)
      (0 until 200).foldLeft(startAt5) { case ((cubeNum, row, col, direction), _) =>
        getNextPosition(cubeNum, row, col, direction)
      } shouldBe startAt5
    }
  }

  @Test
  def walkAroundTheWorldDown(): Unit = {
    val startAt: (Int, Int, Int, Direction) = (0, 35, 15, Down)
    (0 until 200).foldLeft(startAt) { case ((cubeNum, row, col, direction), _) =>
      getNextPosition(cubeNum, row, col, direction)
    } shouldBe startAt
  }

  @Test
  def walkAroundTheWorldLeft(): Unit = {
    val startAt: (Int, Int, Int, Direction) = (0, 35, 15, Left)
    (0 until 200).foldLeft(startAt) { case ((cubeNum, row, col, direction), _) =>
      getNextPosition(cubeNum, row, col, direction)
    } shouldBe startAt
  }

  @Test
  def walkAroundTheWorldRight(): Unit = {
    val startAt: (Int, Int, Int, Direction) = (0, 35, 15, Right)
    (0 until 200).foldLeft(startAt) { case ((cubeNum, row, col, direction), _) =>
      getNextPosition(cubeNum, row, col, direction)
    } shouldBe startAt
  }
}