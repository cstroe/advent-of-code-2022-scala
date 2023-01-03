package aoc2022

import aoc2022.Day22Part2.{Direction, Down, Left, Right, Up, getNextPosition}
import org.junit.Test
import org.scalatest.matchers.should.Matchers

class Day22Part2Test extends Matchers {
  @Test
  def walkAllAroundTheWorld(): Unit = {
    (0 to 5).foreach { testCube =>
      Seq(Up, Down, Left, Right).foreach { testDir =>
        val startAtCross2: (Int, Int, Int, Direction) = (testCube, 49, 0, testDir)
        (0 until 200).foldLeft(startAtCross2) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAtCross2

        val startAtCross1: (Int, Int, Int, Direction) = (testCube, 0, 49, testDir)
        (0 until 200).foldLeft(startAtCross1) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAtCross1

        val startAt49: (Int, Int, Int, Direction) = (testCube, 49, 49, testDir)
        (0 until 200).foldLeft(startAt49) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt49

        val startAt0: (Int, Int, Int, Direction) = (testCube, 0, 0, testDir)
        (0 until 200).foldLeft(startAt0) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt0

        val startAt: (Int, Int, Int, Direction) = (testCube, 35, 15, testDir)
        (0 until 200).foldLeft(startAt) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt

        val startAt1: (Int, Int, Int, Direction) = (testCube, 36, 10, testDir)
        (0 until 200).foldLeft(startAt1) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt1

        val startAt2: (Int, Int, Int, Direction) = (testCube, 46, 20, testDir)
        (0 until 200).foldLeft(startAt2) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt2

        val startAt3: (Int, Int, Int, Direction) = (testCube, 16, 40, testDir)
        (0 until 200).foldLeft(startAt3) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt3

        val startAt4: (Int, Int, Int, Direction) = (testCube, 6, 2, testDir)
        (0 until 200).foldLeft(startAt4) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt4

        val startAt5: (Int, Int, Int, Direction) = (testCube, 20, 49, testDir)
        (0 until 200).foldLeft(startAt5) { case ((cubeNum, row, col, direction), _) =>
          getNextPosition(cubeNum, row, col, direction)
        } shouldBe startAt5
      }
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