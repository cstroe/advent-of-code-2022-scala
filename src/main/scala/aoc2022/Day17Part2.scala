package aoc2022

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

object Day17Part2 {
  class RockStore(var store: Array[Char] = Array.fill(32768)(0),
                  private var maxRow: Long = -1L,
                  private var currentLowIndex: Long = 0L) {
    def add(rock: FallingRock): Unit = {
      rock.points.foreach { point =>
        val rowNum = point.row
        val arrayIndex = (rowNum - currentLowIndex).toInt
        if (arrayIndex >= store.length) { growArray() }
        val existingChar = store(arrayIndex)
        val maskByte: Char = ((0x40) >>> point.col).toChar
        val newChr: Char = (maskByte | existingChar).toChar
        store(arrayIndex) = newChr
      }
      if (rock.location.row > maxRow) {
        maxRow = rock.location.row
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

    def getByRowNum(rowNum: Long): Array[Boolean] = {
      val arrayIndex = (rowNum - currentLowIndex).toInt
      val boolArr = Array.fill(7)(false)

      if (arrayIndex < store.length) {
        val char: Char = store(arrayIndex)

        (0 to 6).foreach { i =>
          if (((0x40 >>> i) & char) != 0x0) {
            boolArr(i) = true
          }
        }
      }

      boolArr
    }

    def get(rock: FallingRock): Array[(Long, Array[Boolean])] = {
      val numRows = rock.highestPoint - rock.lowestPoint + 1
      val rows = Array.ofDim[(Long, Array[Boolean])](numRows.toInt)

      var currentRow = rock.lowestPoint
      var currentIndex = 0
      while(currentRow <= rock.highestPoint) {
        rows(currentIndex) = (currentRow, getByRowNum(currentRow))

        currentRow += 1
        currentIndex += 1
      }
      rows
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

  case class Point(col: Int, row: Long) {
    //lazy val down: Point = Point(col, row - 1)
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

  val bitMasksWithColumn: Array[(Char, Int)] =
    Array((0x40, 0), (0x20, 1), (0x10, 2), (0x08, 3), (0x04, 4), (0x02, 5), (0x01, 6))

  def computePoints(shape: RockShape, location: Point): Set[Point] = {
    shape.encoded
      .map(_ >>> location.col)
      .zipWithIndex.flatMap { case (pointsChar, rowOffset) =>
        bitMasksWithColumn.flatMap { case (bitMask, col) =>
          if ((pointsChar & bitMask) != 0) {
            Option(Point(col, location.row - shape.height + rowOffset + 1))
          } else {
            None
          }
        }
      }.toSet
  }

  def computeChars(shape: RockShape, location: Point): Array[Char] = {
    shape.encoded
      .map(_ >>> location.col).map(_.toChar)
  }

  case class FallingRock(shape: RockShape, location: Point, points: Set[Point], chars: Array[Char]) {

    val highestPoint: Long = location.row
    val lowestPoint: Long = location.row - shape.encoded.length

    def moveLeft: FallingRock = {
      val newPoints: Set[Point] = points.map(p => Point(p.col - 1, p.row))
      val newChars: Array[Char] = chars.map(_ << 1).map(_.toChar)
      FallingRock(shape, Point(location.col - 1, location.row), newPoints, newChars)
    }

    def moveRight: FallingRock = {
      val newPoints: Set[Point] = points.map(p => Point(p.col+1, p.row))
      val newChars: Array[Char] = chars.map(_ >>> 1).map(_.toChar)
      FallingRock(shape, Point(location.col + 1, location.row), newPoints, newChars)
    }

    def moveDown: FallingRock = {
      val newPoints: Set[Point] = points.map(p => Point(p.col, p.row - 1))
      FallingRock(shape, Point(location.col, location.row - 1), newPoints, chars)
    }

    private def isValidMove(room: TallRoom, newLocation: Point, chars: Array[Char]): Boolean = {
      val topRow = newLocation.row.toInt
      val bottomRow = (newLocation.row - chars.length + 1).toInt
      val comparisons = (bottomRow to topRow)
        .map(room.rocks.getCharByRowNum)
        .zipWithIndex
        .map { case (rowChar, i) => (rowChar, chars(i)) }
      val retVal = comparisons.exists { case (rowChar, rockChar) =>
        (rowChar & rockChar) != 0x0
      }
      !retVal
    }

    def canMoveLeft(room: TallRoom): Boolean = {
      if (location.col - 1 < 0) {
        false
      } else {
        isValidMove(room, Point(location.col - 1, location.row), chars.map(c => (c << 1).toChar))
      }
    }

    def canMoveRight(room: TallRoom): Boolean = {
      if (location.col + 1 + shape.width > room.width) {
        false
      } else {
        isValidMove(room, Point(location.col + 1, location.row), chars.map(c => (c >> 1).toChar))
      }
    }

    def canMoveDown(room: TallRoom): Boolean = {
      if ((location.row - 1) - (shape.height - 1) < 0) {
        false
      } else {
        isValidMove(room, Point(location.col, location.row - 1), chars)
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

    def nextSpawnPoint(shape: RockShape): Point = {
      Point(2, height + 3 + (shape.encoded.length - 1))
    }
  }

  def parseInput(fileName: String): Array[Jet] = {
    readFile(s"src/main/resources/day17/$fileName").trim.toCharArray.map {
      case '<' => LeftPush
      case '>' => RightPush
      case _ => throw new RuntimeException("invalid character")
    }
  }

  def placeRock(room: TallRoom, jetsIter: Iterator[Jet], startingRock: FallingRock, debug: Boolean): FallingRock = {
    var rockCanMove = true
    var movingRock: FallingRock = startingRock
    while (rockCanMove) {
      val jet = jetsIter.next()
      if (jet == LeftPush && movingRock.canMoveLeft(room)) {
        movingRock = movingRock.moveLeft
      }
      if (jet == RightPush && movingRock.canMoveRight(room)) {
        movingRock = movingRock.moveRight
      }

      if (movingRock.canMoveDown(room)) {
        movingRock = movingRock.moveDown
      } else {
        rockCanMove = false
      }
    }

    movingRock
  }

  def printTopOfRoom(room: TallRoom, shapeNum: Long): Unit = {
    println(s"Current shape number: $shapeNum")
    ((room.height - 1) to (room.height - 5, -1)).filter(_ >= 0).foreach { rowNum =>
      val row = room.rocks.getByRowNum(rowNum)
      print("|")
      (0 to 6).foreach { colNum =>
        if (row(colNum)) {
          print("█")
        } else {
          print(".")
        }
      }
      println("|")
    }
    println("+-------+")
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

    val printIter = Iterator.unfold(0) { i => Option(
      if (i == jets.length * rockShapes.length) { (0,1) } else { (i, i+1) }
    )}

    var currentIter = 0L
    while(currentIter < iterations) {
      if (printIter.next() == 0) { printTopOfRoom(room, currentIter) }
      val shape = shapesIter.next()
      val newSpawnPoint = room.nextSpawnPoint(shape)
      val rock = FallingRock(shape, newSpawnPoint, computePoints(shape, newSpawnPoint), computeChars(shape, newSpawnPoint))
      val placedRock = placeRock(room, jetsIter, rock, debug = false)
      room.rocks.add(placedRock)
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
