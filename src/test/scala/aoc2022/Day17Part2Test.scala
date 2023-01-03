package aoc2022

import aoc2022.Day17Part2._
import org.junit.{Ignore, Test}
import org.scalatest.matchers.should.Matchers


//noinspection AccessorLikeMethodIsUnit
class Day17Part2Test extends Matchers {
  def newFlatRock(col: Int, row: Long): FallingRock = {
    val shape = FlatRock()
    new FallingRock(shape, row, col, computeChars(shape.encoded, col))
  }

  def newCrossRock(col: Int, row: Long): FallingRock = {
    val shape = CrossRock()
    new FallingRock(shape, row, col, computeChars(shape.encoded, col))
  }

  def strToArray(row: String): Array[Boolean] = row.toCharArray.map {
    case '#' => true
    case _ => false
  }

  @Test
  def rockStoreAcceptsRocks(): Unit = {
    val rockstore = new RockStore()

    // 2 ....#..
    // 1 ...###.
    // 0 #####..
    //   0123456
    val rocks = List(newFlatRock(0, 0), newCrossRock(3,2))

    rocks.foreach(rockstore.add)

    rockstore.getCharByRowNum(2) shouldBe 0x04.toChar
    rockstore.getCharByRowNum(1) shouldBe 0x0e.toChar
    rockstore.getCharByRowNum(0) shouldBe 0x7c
  }

  @Test
  def rockStoreAddingARockIsCorrectlyEncoded(): Unit = {
    val rockstore = new RockStore()

    // 0 ####...
    //   0123456
    rockstore.add(newFlatRock(0, 0))
    rockstore.getCharByRowNum(0) shouldBe 0x78
  }

  @Test
  def rockStoreGrowsWhenAdding(): Unit = {
    val rockstore = new RockStore()
    rockstore.add(newFlatRock(0, 32768))
    rockstore.getCharByRowNum(32768) shouldBe 0x78.toChar
  }

  @Test
  def rockStoreReturnsTallRows(): Unit = {
    val rockstore = new RockStore()
    rockstore.getCharByRowNum(32768) shouldBe 0x00.toChar
  }

  @Test
  def rockStoreIsEmpty(): Unit = {
    val rockstore = new RockStore()
    rockstore shouldBe empty
    rockstore.add(newFlatRock(0, 0))
    rockstore should not be empty
  }

  @Test
  def testInputHeightIsCorrect(): Unit = {
    val jets = parseInput("testinput")
    findHeight(jets, 2022) shouldBe 3068
  }

  @Test
  def testInputHeight1TrillionIsCorrect(): Unit = {
    val jets = parseInput("testinput")
    findHeight(jets, 1_000_000_000_000L) shouldBe 1_514_285_714_288L
  }

  @Test
  def inputHeight1TrillionIsCorrect(): Unit = {
    val jets = parseInput("input")
    findHeight(jets, 1_000_000_000_000L) shouldBe 1_581_449_275_319L
  }

  @Test
  def inputHeightIsCorrect(): Unit = {
    val jets = parseInput("input")
    findHeight(jets, 2022) shouldBe 3157
  }

  @Test
  def moveDownCheckIsCorrect(): Unit = {
    val room = new TallRoom(new RockStore())
    val flatRock = newFlatRock(0, 0)
    flatRock.canMoveDown(room) shouldBe false
    room.rocks.add(flatRock)
    newCrossRock(0, 3).canMoveDown(room) shouldBe false
    newCrossRock(3, 3).canMoveDown(room) shouldBe true
  }

  @Test
  def moveDownCheckWithOldOne(): Unit = {
    val room = new TallRoom(new RockStore())
    val flatRock = newFlatRock(0, 0)
    flatRock.canMoveDown(room) shouldBe false
    room.rocks.add(flatRock)
  }

  @Test
  def getTopN(): Unit = {
    val room = new TallRoom(new RockStore())
    room.rocks.add(newFlatRock(0, 0))
    room.rocks.add(newFlatRock(0, 1))
    room.rocks.add(newCrossRock(0, 4))
    room.rocks.getTop(3) shouldBe empty
    room.height shouldBe 5
    room.rocks.getMaxRow shouldBe 4
    val expectedTop5 =
      """
        |.█.....| 4
        |███....| 3
        |.█.....| 2
        |████...| 1
        |████...| 0
        +-------+
      """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"
    room.rocks.printTop(4, 5) shouldBe expectedTop5

    room.rocks.add(newFlatRock(3, 3))
    val expectedTop5_2 =
      """
        |.█.....| 4
        |███████| 3
        |.█.....| 2
        |████...| 1
        |████...| 0
        +-------+
      """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"
    room.rocks.printTop(4, 5) shouldBe expectedTop5_2
    val expectedTop3 = Array(0x20.toChar, 0x7F.toChar, 0x20.toChar).mkString("")
    room.rocks.getTop(3) shouldBe Option(expectedTop3)
  }

  @Test
  def movingTheFlatRock(): Unit = {
    val rock = newFlatRock(0,10)
    rock.col shouldBe 0
    rock.chars(0) shouldBe (0x78).toChar // ### #...

    rock.moveDown()
    rock.row shouldBe 9

    rock.moveRight()
    rock.col shouldBe 1
    rock.chars.length shouldBe 1
    rock.chars(0) shouldBe (0x3C).toChar // .## ##..
    rock.moveRight()
    rock.col shouldBe 2
    rock.chars.length shouldBe 1
    rock.chars(0) shouldBe (0x1E).toChar // ..# ###..
    rock.moveRight()
    rock.col shouldBe 3
    rock.chars.length shouldBe 1
    rock.chars(0) shouldBe (0x0F).toChar // ... ####

    rock.moveLeft()
    rock.col shouldBe 2
    rock.chars.length shouldBe 1
    rock.chars(0) shouldBe (0x1E).toChar
    rock.moveLeft()
    rock.col shouldBe 1
    rock.chars.length shouldBe 1
    rock.chars(0) shouldBe (0x3C).toChar
    rock.moveLeft()
    rock.col shouldBe 0
    rock.chars.length shouldBe 1
    rock.chars(0) shouldBe (0x78).toChar
  }

  @Test
  def movingTheLRock(): Unit = {
    val shape = LRock()
    val rock = new FallingRock(shape, 10, 0, computeChars(shape.encoded, 0))
    rock.col shouldBe 0
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x10).toChar // ..# ....
    rock.chars(1) shouldBe (0x10).toChar // ..# ....
    rock.chars(0) shouldBe (0x70).toChar // ### ....

    rock.moveDown()
    rock.row shouldBe 9
    rock.chars(2) shouldBe (0x10).toChar // ..# ....
    rock.chars(1) shouldBe (0x10).toChar // ..# ....
    rock.chars(0) shouldBe (0x70).toChar // ### ....

    rock.moveRight()
    rock.col shouldBe 1
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x08).toChar // ... #...
    rock.chars(1) shouldBe (0x08).toChar // ... #...
    rock.chars(0) shouldBe (0x38).toChar // .## #...
    rock.moveRight()
    rock.col shouldBe 2
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x04).toChar // ... .#..
    rock.chars(1) shouldBe (0x04).toChar // ... .#..
    rock.chars(0) shouldBe (0x1C).toChar // ..# ##..
    rock.moveRight()
    rock.col shouldBe 3
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x02).toChar // ... ..#.
    rock.chars(1) shouldBe (0x02).toChar // ... ..#.
    rock.chars(0) shouldBe (0x0E).toChar // ... ###.
    rock.moveRight()
    rock.col shouldBe 4
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x01).toChar // ... ...#
    rock.chars(1) shouldBe (0x01).toChar // ... ...#
    rock.chars(0) shouldBe (0x07).toChar // ... .###

    rock.moveLeft()
    rock.col shouldBe 3
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x02).toChar // ... ..#.
    rock.chars(1) shouldBe (0x02).toChar // ... ..#.
    rock.chars(0) shouldBe (0x0E).toChar // ... ###.
    rock.moveLeft()
    rock.col shouldBe 2
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x04).toChar // ... .#..
    rock.chars(1) shouldBe (0x04).toChar // ... .#..
    rock.chars(0) shouldBe (0x1C).toChar // ..# ##..
    rock.moveLeft()
    rock.col shouldBe 1
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x08).toChar // ... #...
    rock.chars(1) shouldBe (0x08).toChar // ... #...
    rock.chars(0) shouldBe (0x38).toChar // .## #...
    rock.moveLeft()
    rock.col shouldBe 0
    rock.chars.length shouldBe 3
    rock.chars(2) shouldBe (0x10).toChar // ..# ....
    rock.chars(1) shouldBe (0x10).toChar // ..# ....
    rock.chars(0) shouldBe (0x70).toChar // ### ....
  }

}