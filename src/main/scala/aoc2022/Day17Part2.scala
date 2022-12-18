package aoc2022

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day17Part2 {
  class PartitionBuffer(sep: Long,
                        map: mutable.HashMap[Long, Array[Boolean]],
                        private var maxRow: Long = 0) {
    def add(rock: FallingRock): Unit = {
      rock.points.foreach { point =>
        getByRowNum(point.row)(point.col) = true
      }
      if (rock.location.row > maxRow) {
        maxRow = rock.location.row
      }
    }

    def getByRowNum(rowNum: Long): Array[Boolean] = {
      map.getOrElse(rowNum, {
        val newBuffer = Array.fill(7)(false)
        map.put(rowNum, newBuffer)
        newBuffer
      })
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

    def isEmpty: Boolean = map.isEmpty
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
    def encoded: String
    def width: Int
    def height: Int
  }

  sealed case class FlatRock(encoded: String = "####............") extends RockShape {
    override def width: Int = 4
    override def height: Int = 1
    override def toString: String = getClass.getSimpleName
  }

  sealed case class CrossRock(encoded: String = ".#..###..#......") extends RockShape {
    override def width: Int = 3

    override def height: Int = 3

    override def toString: String = getClass.getSimpleName
  }

  sealed case class LRock(encoded: String = "..#...#.###.....") extends RockShape {
    override def width: Int = 3

    override def height: Int = 3

    override def toString: String = getClass.getSimpleName
  }

  sealed case class TallRock(encoded: String = "#...#...#...#...") extends RockShape {
    override def width: Int = 1

    override def height: Int = 4

    override def toString: String = getClass.getSimpleName
  }

  sealed case class BlockRock(encoded: String = "##..##..........") extends RockShape {
    override def width: Int = 2

    override def height: Int = 2

    override def toString: String = getClass.getSimpleName
  }

  val rockShapes: List[RockShape] = List(
    FlatRock(), CrossRock(), LRock(), TallRock(), BlockRock()
  )

  def computePoints(shape: RockShape, location: Point): Set[Point] = {
    val rows: List[String] = shape.encoded.grouped(4).toList
    rows.zipWithIndex.flatMap { case (encodedRow, rowNum) =>
      encodedRow.grouped(1).toList.zipWithIndex.flatMap { case (encodedDot, colNum) =>
        encodedDot match {
          case "#" => Option(Point(location.col + colNum, location.row - rowNum))
          case _ => None
        }
      }
    }.toSet
  }

  case class FallingRock(shape: RockShape, location: Point, points: Set[Point]) {

    val highestPoint: Long = location.row
    val lowestPoint: Long = location.row - shape.height

    val bottomPoints: Array[Point] = shape match {
      case FlatRock(_) =>
        Array.tabulate(4) { i => Point(location.col + i, location.row - 1) }
      case CrossRock(_) =>
        Array.tabulate(3) { i =>
          if (i == 0 || i == 2) { Point(location.col + i, location.row - 2) }
          else { Point(location.col + i, location.row - 3) }
        }
      case LRock(_) =>
        Array.tabulate(3) { i => Point(location.col + i, location.row - 3) }
      case TallRock(_) =>
        Array.tabulate(1) { _ => Point(location.col, location.row - 4) }
      case BlockRock(_) =>
        Array.tabulate(2) { i => Point(location.col + i, location.row - 2) }
    }

    val leftPoints: Array[Point] = shape match {
      case FlatRock(_) =>
        Array.tabulate(1) { _ => Point(location.col - 1, location.row) }
      case CrossRock(_) =>
        Array.tabulate(3) { i =>
          if (i == 0 || i == 2) { Point(location.col, location.row - i) }
          else { Point(location.col - 1, location.row - i) }
        }
      case LRock(_) =>
        Array.tabulate(3) { i =>
          if (i < 2) { Point(location.col + 1, location.row - i) }
          else { Point(location.col - 1, location.row - i) }
        }
      case TallRock(_) =>
        Array.tabulate(4) { i => Point(location.col - 1, location.row - i) }
      case BlockRock(_) =>
        Array.tabulate(2) { i => Point(location.col - 1, location.row - i) }
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
        checkForIntersection(room, newRock)
      }
    }

    def canMoveDown(room: TallRoom): Boolean = {
      if ((location.row - 1) - (shape.height - 1) < 0) {
        false
      } else {
        checkForIntersection(room, bottomPoints)
      }
    }
  }

  class TallRoom(val rocks: PartitionBuffer) {
    def height: Long = if (rocks.isEmpty) {
      0
    } else {
      rocks.getMaxRow + 1L
    }

    def width: Int = 7

    def nextSpawnPoint(shape: RockShape): Point = {
      Point(2, height + 3 + (shape.height - 1))
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

  def placeRock(room: TallRoom, jets: Array[Jet], startingRock: FallingRock, jetsIndex: Int, debug: Boolean): (FallingRock, Int) = {
    var rockCanMove = true
    var currentJetsIndex = jetsIndex
    var movingRock: FallingRock = startingRock
    if (debug) {
      println("A new rock beings to fall")
      room.showItWith(movingRock)
    }
    while (rockCanMove) {

      if (currentJetsIndex >= jets.length) {
        currentJetsIndex = 0
      }
      val jet = jets(currentJetsIndex)
      currentJetsIndex += 1
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

    (movingRock, currentJetsIndex)
  }

  def main(args: Array[String]): Unit = {
    val startTime = ZonedDateTime.now

    val sep = 25
    val jets = parseInput("input")
    val room = new TallRoom(rocks = new PartitionBuffer(sep, map = new mutable.HashMap()))

    val rolloverAt = jets.length * rockShapes.length
    println(s"Shapes and Jets roll over at: ${jets.length * rockShapes.length}")

    var currentShapeIndex = 0
    var rolloverCounter = 0
    var jetsIndex = 0
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
      val (placedRock, endingJetsIndex) = placeRock(room, jets, rock, jetsIndex, debug = false)
      jetsIndex = endingJetsIndex
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
