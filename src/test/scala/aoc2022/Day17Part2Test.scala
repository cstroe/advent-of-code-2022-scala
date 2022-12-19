package aoc2022

import aoc2022.Day17Part2.{CrossRock, FallingRock, FlatRock, Point, RockShape, RockStore, TallRoom, computeChars, computePoints, findHeight, parseInput, rockShapes}
import org.junit.Test
import org.junit.Assert.{assertArrayEquals, assertFalse, assertTrue}


class Day17Part2Test {
  def newFlatRock(col: Int, row: Long): FallingRock = {
    val shape = FlatRock()
    val location = Point(col, row)
    FallingRock(shape, location, computePoints(shape, location), computeChars(shape, location))
  }

  def newCrossRock(col: Int, row: Long): FallingRock = {
    val shape = CrossRock()
    val location = Point(col, row)
    FallingRock(shape, location, computePoints(shape, location), computeChars(shape, location))
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

    assertArrayEquals(strToArray("....#.."), rockstore.getByRowNum(2))
    assertArrayEquals(strToArray("...###."), rockstore.getByRowNum(1))
    assertArrayEquals(strToArray("#####.."), rockstore.getByRowNum(0))
  }

  @Test
  def rockStoreAddingARockIsCorrectlyEncoded(): Unit = {
    val rockstore = new RockStore()

    // 0 ####...
    //   0123456
    rockstore.add(newFlatRock(0, 0))
    assertTrue(rockstore.getCharByRowNum(0) == 0x78)
  }

  @Test
  def rockStoreGrowsWhenAdding(): Unit = {
    val rockstore = new RockStore()
    rockstore.add(newFlatRock(0, 32768))
    assertArrayEquals(strToArray("####..."), rockstore.getByRowNum(32768))
  }

  @Test
  def rockStoreReturnsTallRows(): Unit = {
    val rockstore = new RockStore()
    assertArrayEquals(strToArray("......."), rockstore.getByRowNum(32768))
  }

  @Test
  def rockStoreIsEmpty(): Unit = {
    val rockstore = new RockStore()
    assertTrue(rockstore.isEmpty)
    rockstore.add(newFlatRock(0, 0))
    assertFalse(rockstore.isEmpty)
  }

  @Test
  def testInputHeightIsCorrect(): Unit = {
    val jets = parseInput("testinput")
    val height = findHeight(jets, 2022)
    val expectedHeight = 3068
    assertTrue(s"Height should be $expectedHeight, but it is $height", height == expectedHeight)
  }

  @Test
  def inputHeightIsCorrect(): Unit = {
    val jets = parseInput("input")
    val height = findHeight(jets, 2022)
    val expectedHeight = 3157
    assertTrue(s"Height should be $expectedHeight, but it is $height", height == expectedHeight)
  }

  @Test
  def moveDownCheckIsCorrect(): Unit = {
    val room = new TallRoom(new RockStore())
    val flatRock = newFlatRock(0, 0)
    assertFalse(flatRock.canMoveDown(room))
    room.rocks.add(flatRock)
    assertFalse(newCrossRock(0, 3).canMoveDown(room))
    assertTrue(newCrossRock(3, 3).canMoveDown(room))
  }

  @Test
  def moveDownCheckWithOldOne(): Unit = {
    val room = new TallRoom(new RockStore())
    val flatRock = newFlatRock(0, 0)
    assertFalse(flatRock.canMoveDown(room))
    room.rocks.add(flatRock)
  }
}