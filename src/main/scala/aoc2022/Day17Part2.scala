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
        val chr: Char = store(arrayIndex)
        val newChr: Char = ((0x1 << point.col) | chr).toChar
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

    def getByRowNum(rowNum: Long): Array[Boolean] = {
      val arrayIndex = (rowNum - currentLowIndex).toInt
      val boolArr = Array.fill(7)(false)

      if (arrayIndex < store.length) {
        val char: Char = store(arrayIndex)

        (0 to 6).foreach { i =>
          if (((0x01 << i) & char) != 0x0) {
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

  case class FallingRock(shape: RockShape, location: Point, points: Set[Point]) {

    val highestPoint: Long = location.row
    val lowestPoint: Long = location.row - shape.encoded.length

    val bottomPoints: Array[Point] = shape match {
      case FlatRock(_, _) =>
        Array.tabulate(4) { i => Point(location.col + i, location.row - 1) }
      case CrossRock(_, _) =>
        Array.tabulate(3) { i =>
          if (i == 0 || i == 2) { Point(location.col + i, location.row - 2) }
          else { Point(location.col + i, location.row - 3) }
        }
      case LRock(_, _) =>
        Array.tabulate(3) { i => Point(location.col + i, location.row - 3) }
      case TallRock(_, _) =>
        Array.tabulate(1) { _ => Point(location.col, location.row - 4) }
      case BlockRock(_, _) =>
        Array.tabulate(2) { i => Point(location.col + i, location.row - 2) }
    }

    val bottomPointsArray: Array[Long] = shape match {
      case FlatRock(_, _) =>
        Array(
          location.col, location.row - 1,
          location.col + 1, location.row - 1,
          location.col + 2, location.row - 1,
          location.col + 3, location.row - 1,
        )
      case CrossRock(_, _) =>
        Array(
          location.col, location.row - 2,
          location.col + 1, location.row - 3,
          location.col + 2, location.row - 2,
        )
      case LRock(_, _) =>
        Array(
          location.col, location.row - 3,
          location.col + 1, location.row - 3,
          location.col + 2, location.row - 3,
        )
      case TallRock(_, _) =>
        Array(location.col, location.row - 4)
      case BlockRock(_, _) =>
        Array(
          location.col, location.row - 2,
          location.col + 1, location.row - 2,
        )
    }

    val leftPoints: Array[Point] = shape match {
      case FlatRock(_, _) =>
        Array.tabulate(1) { _ => Point(location.col - 1, location.row) }
      case CrossRock(_, _) =>
        Array.tabulate(3) { i =>
          if (i == 0 || i == 2) { Point(location.col, location.row - i) }
          else { Point(location.col - 1, location.row - i) }
        }
      case LRock(_, _) =>
        Array.tabulate(3) { i =>
          if (i < 2) { Point(location.col + 1, location.row - i) }
          else { Point(location.col - 1, location.row - i) }
        }
      case TallRock(_, _) =>
        Array.tabulate(4) { i => Point(location.col - 1, location.row - i) }
      case BlockRock(_, _) =>
        Array.tabulate(2) { i => Point(location.col - 1, location.row - i) }
    }

    val rightPoints: Array[Point] = shape match {
      case FlatRock(_, _) =>
        Array.tabulate(1) { _ => Point(location.col + 4, location.row) }
      case CrossRock(_, _) =>
        Array.tabulate(3) { i =>
          if (i == 0 || i == 2) {
            Point(location.col + 2, location.row - i)
          }
          else {
            Point(location.col + 3, location.row - i)
          }
        }
      case LRock(_, _) =>
        Array.tabulate(3) { i => Point(location.col + 3, location.row - i) }
      case TallRock(_, _) =>
        Array.tabulate(4) { i => Point(location.col + 1, location.row - i) }
      case BlockRock(_, _) =>
        Array.tabulate(2) { i => Point(location.col + 2, location.row - i) }
    }

    def intersects(row: (Long, Array[Boolean])): Boolean = {
      points.exists(point => point.row == row._1 && row._2(point.col))
    }

    def moveLeft: FallingRock = {
      val newPoints: Set[Point] = points.map(p => Point(p.col - 1, p.row))
      FallingRock(shape, Point(location.col - 1, location.row), newPoints)
    }

    def moveRight: FallingRock = {
      val newPoints: Set[Point] = points.map(p => Point(p.col+1, p.row))
      FallingRock(shape, Point(location.col + 1, location.row), newPoints)
    }

    def moveDown: FallingRock = {
      val newPoints: Set[Point] = points.map(p => Point(p.col, p.row - 1))
      FallingRock(shape, Point(location.col, location.row - 1), newPoints)
    }

    private def checkForIntersection(room: TallRoom, newRock: FallingRock): Boolean = {
      !room.rocks.get(newRock).exists(row => newRock.intersects(row))
    }

    private def checkForIntersection(room: TallRoom, points: Array[Point]): Boolean = {
      !points.exists(p => room.rocks.getByRowNum(p.row)(p.col))
    }

    private def checkForIntersection(room: TallRoom, points: Array[Long]): Boolean = {
      var currentIndex = 0
      var foundIntersection = false
      while (!foundIntersection && currentIndex < points.length) {
        val col = points(currentIndex).toInt
        val row = points(currentIndex+1)
        foundIntersection = room.rocks.getByRowNum(row)(col)
        currentIndex += 2
      }
      !foundIntersection
    }

    def canMoveLeft(room: TallRoom): Boolean = {
      if (location.col - 1 < 0) {
        false
      } else {
        checkForIntersection(room, leftPoints)
      }
    }

    def canMoveRight(room: TallRoom): Boolean = {
      val newRock = moveRight
      if (newRock.location.col + newRock.shape.width > room.width) {
        false
      } else {
        checkForIntersection(room, rightPoints)
      }
    }

    def canMoveDown(room: TallRoom): Boolean = {
      if ((location.row - 1) - (shape.encoded.length - 1) < 0) {
        false
      } else {
        checkForIntersection(room, bottomPointsArray)
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

    def showIt(): Unit = {
      ???
    }

    def showItWith(rock: FallingRock): Unit = {
      ???
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
    if (debug) {
      println("A new rock beings to fall")
      room.showItWith(movingRock)
    }
    while (rockCanMove) {
      val jet = jetsIter.next()
      if (debug) {
        println(s"Current jet: $jet")
      }
      if (jet == LeftPush && movingRock.canMoveLeft(room)) {
        if (debug) {
          println("< (move left)")
        }
        movingRock = movingRock.moveLeft
        if (debug) {
          room.showItWith(movingRock)
        }
      }
      if (jet == RightPush && movingRock.canMoveRight(room)) {
        if (debug) {
          println("> (move right)")
        }
        movingRock = movingRock.moveRight
        if (debug) {
          room.showItWith(movingRock)
        }
      }

      if (movingRock.canMoveDown(room)) {
        if (debug) {
          println(". (move down)")
        }
        movingRock = movingRock.moveDown
        if (debug) {
          room.showItWith(movingRock)
        }
      } else {
        rockCanMove = false
      }
    }

    movingRock
  }

  def main(args: Array[String]): Unit = {
    val startTime = ZonedDateTime.now

    val jets = parseInput("input")
    val room = new TallRoom(rocks = new RockStore())

    val rolloverAt = jets.length * rockShapes.length
    println(s"Shapes and Jets roll over at: ${jets.length * rockShapes.length}")

    var currentShapeIndex = 0
    var rolloverCounter = 0

    val jetsIter = Iterator.unfold(0) { i =>
      if (i >= jets.length) {
        Option((jets(0), 1))
      } else { Option(jets(i), i+1) }
    }

    (0 until 10_000_000).foreach { rockNum =>
      if (rolloverCounter == rolloverAt) {
        rolloverCounter = 0
        println(s"Rolling over at: $rockNum")
        ((room.height - 1) to (room.height - 5, -1)).foreach { rowNum =>
          val row = room.rocks.getByRowNum(rowNum)
          print("|")
          (0 to 6).foreach { colNum =>
            if (row(colNum)) { print("█") } else { print(".") }
          }
          println("|")
        }
        println("+-------+")
      }
      rolloverCounter += 1

      if (currentShapeIndex == rockShapes.length) {
        currentShapeIndex = 0
      }
      val shape = rockShapes(currentShapeIndex)
      currentShapeIndex += 1

      val newSpawnPoint = room.nextSpawnPoint(shape)
      val rock = FallingRock(shape, newSpawnPoint, computePoints(shape, newSpawnPoint))
      val placedRock = placeRock(room, jetsIter, rock, debug = false)
      room.rocks.add(placedRock)

      //room.showIt()
    }

    println(s"Room height: ${room.height}")

    val durationSections = startTime.until(ZonedDateTime.now, ChronoUnit.SECONDS)
    println(s"Execution took $durationSections seconds")

    val expectedHeight = 15814463
    assert(room.height == expectedHeight, s"Room height ${room.height} != $expectedHeight")
  }
}
