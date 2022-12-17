package aoc2022

import org.apache.commons.lang3.time.StopWatch

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day17Part2 {
  class PartitionBuffer(sep: Long,
                        map: mutable.HashMap[Long, mutable.ArrayBuffer[FallingRock]],
                        private var maxRow: Long = 0) {
    private def computePartition(rock: FallingRock): Long = {
      var row = rock.location.row
      while (row % sep != 0) { row += 1 }
      row
    }

    def add(rock: FallingRock): Unit = {
      get(rock).addOne(rock)
      if (rock.location.row > maxRow) {
        maxRow = rock.location.row
      }
    }

    private def getByPartition(partition: Long): mutable.ArrayBuffer[FallingRock] = {
      map.getOrElse(partition, {
        val newBuffer = new ArrayBuffer[FallingRock]()
        map.put(partition, newBuffer)
        newBuffer
      })
    }

    def get(rock: FallingRock): mutable.ArrayBuffer[FallingRock] = {
      val partition = computePartition(rock)
      getByPartition(partition)
    }

    def getBelow(rock: FallingRock): mutable.ArrayBuffer[FallingRock] = {
      val partition = computePartition(rock)
      if (partition > sep) {
        val partitionBelow = partition - sep
        getByPartition(partitionBelow)
      } else {
        getByPartition(partition)
      }
    }

    def getAbove(rock: FallingRock): mutable.ArrayBuffer[FallingRock] = {
      val partition = computePartition(rock) + sep
      getByPartition(partition)
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

  case class Point(col: Int, row: Long)

  trait RockShape {
    def encoded: String

    def width: Int

    def height: Int
  }

  case class FlatRock(encoded: String = "####............") extends RockShape {
    override def width: Int = 4

    override def height: Int = 1

    override def toString: String = getClass.getSimpleName
  }

  case class CrossRock(encoded: String = ".#..###..#......") extends RockShape {
    override def width: Int = 3

    override def height: Int = 3

    override def toString: String = getClass.getSimpleName
  }

  case class LRock(encoded: String = "..#...#.###.....") extends RockShape {
    override def width: Int = 3

    override def height: Int = 3

    override def toString: String = getClass.getSimpleName
  }

  case class TallRock(encoded: String = "#...#...#...#...") extends RockShape {
    override def width: Int = 1

    override def height: Int = 4

    override def toString: String = getClass.getSimpleName
  }

  case class BlockRock(encoded: String = "##..##..........") extends RockShape {
    override def width: Int = 2

    override def height: Int = 2

    override def toString: String = getClass.getSimpleName
  }

  val rockShapes: List[RockShape] = List(
    FlatRock(), CrossRock(), LRock(), TallRock(), BlockRock()
  )

  case class FallingRock(shape: RockShape, location: Point) {
    def toPoints(): Set[Point] = {
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

    def intersects(rock: FallingRock): Boolean = {
      toPoints().intersect(rock.toPoints()).nonEmpty
    }

    def moveLeft: FallingRock = FallingRock(shape, Point(location.col - 1, location.row))

    def moveRight: FallingRock = FallingRock(shape, Point(location.col + 1, location.row))

    def moveDown: FallingRock = FallingRock(shape, Point(location.col, location.row - 1))

    private def checkForIntersection(room: TallRoom, newRock: FallingRock): Boolean = {
      !room.rocks.get(newRock).exists(rock => newRock.intersects(rock)) &&
        !room.rocks.getBelow(newRock).exists(rock => newRock.intersects(rock)) &&
        !room.rocks.getAbove(newRock).exists(rock => newRock.intersects(rock))
    }

    def canMoveLeft(room: TallRoom): Boolean = {
      val newRock = moveLeft
      if (newRock.location.col < 0) {
        false
      } else {
        checkForIntersection(room, newRock)
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
      val newRock = moveDown
      if (newRock.location.row - (newRock.shape.height - 1) < 0) {
        false
      } else {
        checkForIntersection(room, newRock)
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

  def parseInput(fileName: String): List[Jet] = {
    readFile(s"src/main/resources/day17/$fileName").trim.toCharArray.map {
      case '<' => LeftPush
      case '>' => RightPush
      case _ => throw new RuntimeException("invalid character")
    }.toList
  }

  def placeRock(room: TallRoom, jets: List[Jet], startingRock: FallingRock, startingMoveNum: Int, debug: Boolean): (FallingRock, Int) = {
    var rockCanMove = true
    var moveNum = startingMoveNum
    var movingRock: FallingRock = startingRock
    if (debug) {
      println("A new rock beings to fall")
      room.showItWith(movingRock)
    }
    while (rockCanMove) {
      val jet = jets(moveNum % jets.size)
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

      moveNum += 1
    }

    (movingRock, moveNum)
  }

  def main(args: Array[String]): Unit = {
    val startTime = ZonedDateTime.now

    val sep = 25
    val jets = parseInput("input")
    val room = new TallRoom(rocks = new PartitionBuffer(sep, map = new mutable.HashMap()))

    var moveNum = 0

    var currentShapeIndex = 0
    var printCounter = 0
    (0 until 1_000_000).foreach { rockNum =>
      if (printCounter == 1000) {
        printCounter = 0
      }
      println(s"Rock number: $rockNum")
      printCounter += 1

      if (currentShapeIndex == rockShapes.length) {
        currentShapeIndex = 0
      }
      val shape = rockShapes(currentShapeIndex)
      currentShapeIndex += 1

      println(shape)
      val newSpawnPoint = room.nextSpawnPoint(shape)
      val rock = FallingRock(shape, newSpawnPoint)
      val (placedRock, endingMoveNum) = placeRock(room, jets, rock, moveNum, debug = false)
      moveNum = endingMoveNum
      room.rocks.add(placedRock)
      //room.showIt()
    }

    println(s"Room height: ${room.height}")
    assert(room.height == 3157)

    val durationSections = startTime.until(ZonedDateTime.now, ChronoUnit.SECONDS)
    println(s"Execution took $durationSections seconds")
  }
}
