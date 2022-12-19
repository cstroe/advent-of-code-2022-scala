package aoc2022

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

object Day17Part2 {
  class RockStore(var store: Array[Char] = Array.fill(32768)(0),
                  private var maxRow: Long = -1L,
                  private var currentLowIndex: Long = 0L) {
    def add(rock: FallingRock): Unit = {
      val topRow = rock.row
      val bottomRow = rock.row - rock.chars.length + 1
      (bottomRow to topRow).zipWithIndex.foreach { case (rowNum, i) =>
        val storeArrayIndex = (rowNum - currentLowIndex).toInt
        if (storeArrayIndex >= store.length) { growArray() }
        val existingChar = store(storeArrayIndex)
        val maskByte: Char = rock.chars(i)
        val newChr: Char = (maskByte | existingChar).toChar
        store(storeArrayIndex) = newChr
      }
      if (rock.row > maxRow) {
        maxRow = rock.row
      }
    }

    private def growArray(): Unit = {
      val newSize = store.length * 2
      val newStore: Array[Char] = Array.fill(newSize)(0x0)
      Array.copy(store, 0, newStore, 0, store.length)
      store = newStore
    }

    def getCharByRowNum(rowNum: Int): Char = {
      if (rowNum >= store.length) { 0x00 }
      else if (rowNum < 0) { 0xFF }
      else { store(rowNum) }
    }

    def isEmpty: Boolean = maxRow == -1L
    def getMaxRow: Long = maxRow
  }


  sealed trait Jet

  case object LeftPush extends Jet {
    override def toString: String = "<"
  }

  case object RightPush extends Jet {
    override def toString: String = ">"
  }

  sealed trait RockShape {
    def encoded: Array[Char]
    def width: Int
    def height: Int = encoded.length
    override def toString: String = getClass.getSimpleName
  }

  // ####...
  sealed case class FlatRock(encoded: Array[Char] = Array(0x78), width: Int = 4) extends RockShape

  // .#..... 0x20
  // ###.... 0x70
  // .#..... 0x20
  sealed case class CrossRock(encoded: Array[Char] = Array(0x20, 0x70, 0x20),
                              width: Int = 3) extends RockShape
  // ..#.... 0x10
  // ..#.... 0x10
  // ###.... 0x70
  sealed case class LRock(encoded: Array[Char] = Array(0x70, 0x10, 0x10),
                          width: Int = 3) extends RockShape

  // #...... 0x40
  // #...... 0x40
  // #...... 0x40
  // #...... 0x40
  sealed case class TallRock(encoded: Array[Char] = Array(0x40, 0x40, 0x40, 0x40),
                             width: Int = 1) extends RockShape

   // ##..... 0x60
   // ##..... 0x60
  sealed case class BlockRock(encoded: Array[Char] = Array(0x60, 0x60),
                              width: Int = 2) extends RockShape

  val rockShapes: List[RockShape] = List(
    FlatRock(), CrossRock(), LRock(), TallRock(), BlockRock()
  )

  def computeChars(shape: RockShape, col: Int): Array[Char] =
    shape.encoded.map(_ >>> col).map(_.toChar)

  class FallingRock(var shape: RockShape, var row: Long, var chars: Array[Char]) {
    def moveLeft(): Unit = { chars = chars.map(_ << 1).map(_.toChar) }
    def moveRight(): Unit = { chars = chars.map(_ >>> 1).map(_.toChar) }
    def moveDown(): Unit = { row -= 1 }

    private def isValidMove(room: TallRoom, row: Long, chars: Array[Char]): Boolean = {
      val topRow = row.toInt
      val bottomRow = (row - chars.length + 1).toInt

      var currentRow = bottomRow
      var currentIndex = 0
      var retVal = true
      while(retVal && currentRow <= topRow) {
        val rowChar = room.rocks.getCharByRowNum(currentRow)
        val rockChar = chars(currentIndex)
        if ((rowChar & rockChar) != 0x0) {
          retVal = false
        }

        currentRow += 1
        currentIndex += 1
      }
      retVal
    }

    def canMoveLeft(room: TallRoom): Boolean = {
      if (chars.exists(c => (c & 0x40) != 0)) {
        false
      } else {
        isValidMove(room, row, chars.map(c => (c << 1).toChar))
      }
    }

    def canMoveRight(room: TallRoom): Boolean = {
      if (chars.exists(c => (c & 0x01) != 0)) {
        false
      } else {
        isValidMove(room, row, chars.map(c => (c >> 1).toChar))
      }
    }

    def canMoveDown(room: TallRoom): Boolean = {
      if ((row - 1) - (shape.height - 1) < 0) {
        false
      } else {
        isValidMove(room, row - 1, chars)
      }
    }
  }

  class TallRoom(val rocks: RockStore) {
    def height: Long = if (rocks.isEmpty) {
      0
    } else {
      rocks.getMaxRow + 1L
    }

    def width: Int = 7

    def nextSpawnRow(shape: RockShape): Long = height + 3 + (shape.encoded.length - 1)
  }

  def parseInput(fileName: String): Array[Jet] = {
    readFile(s"src/main/resources/day17/$fileName").trim.toCharArray.map {
      case '<' => LeftPush
      case '>' => RightPush
      case _ => throw new RuntimeException("invalid character")
    }
  }

  def placeRock(room: TallRoom, jetsIter: Iterator[Jet], rock: FallingRock, debug: Boolean): Unit = {
    var rockCanMove = true
    while (rockCanMove) {
      val jet = jetsIter.next()
      if (jet == LeftPush && rock.canMoveLeft(room)) {
        rock.moveLeft()
      }
      if (jet == RightPush && rock.canMoveRight(room)) {
        rock.moveRight()
      }

      if (rock.canMoveDown(room)) {
        rock.moveDown()
      } else {
        rockCanMove = false
      }
    }
  }

  def findHeight(jets: Array[Jet], iterations: Long): Long = {
    val room = new TallRoom(rocks = new RockStore())

    val shapesIter = Iterator.unfold(0) { i =>
      if (i >= rockShapes.length) {
        Option(rockShapes.head, 1)
      } else {
        Option(rockShapes(i), i + 1)
      }
    }

    val jetsIter = Iterator.unfold(0) { i =>
      if (i >= jets.length) {
        Option((jets(0), 1))
      } else {
        Option(jets(i), i + 1)
      }
    }

    val printIter = Iterator.unfold(0) { i => Option(if (i == 100_000) { (0, 1) } else { (i, i+1) }) }

    var currentIter = 0L
    val rock = new FallingRock(FlatRock(), 0, Array.empty) // this object will be mutated
    while(currentIter < iterations) {
      if (printIter.next() == 0) { println(currentIter) }
      rock.shape = shapesIter.next()
      rock.row = room.nextSpawnRow(rock.shape)
      rock.chars = computeChars(rock.shape, 2)
      placeRock(room, jetsIter, rock, debug = false)
      room.rocks.add(rock)
      currentIter += 1
    }

    room.height
  }

  def main(args: Array[String]): Unit = {
    val startTime = ZonedDateTime.now

    val jets = parseInput("input")
    val height = findHeight(jets, 10_000_000)

    println(s"Room height: $height")

    val durationSections = startTime.until(ZonedDateTime.now, ChronoUnit.SECONDS)
    println(s"Execution took $durationSections seconds")
  }
}
