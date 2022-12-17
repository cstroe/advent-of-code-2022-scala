package aoc2022

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day17Part2 {
  class PartitionBuffer(sep: Long,
                        map: mutable.HashMap[Long, mutable.ArrayBuffer[FallingRock]],
                        private var maxRow: Long = 0) {
    def add(rock: FallingRock): Unit = {
      get().addOne(rock)
      if (rock.location.row > maxRow) {
        maxRow = rock.location.row
      }
    }

    def get(): mutable.ArrayBuffer[FallingRock] = {
      map.getOrElse(sep, {
        val newBuffer = new ArrayBuffer[FallingRock]()
        map.put(sep, newBuffer)
        newBuffer
      })
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

    def canMoveLeft(room: TallRoom): Boolean = {
      val newRock = moveLeft
      if (newRock.location.col < 0) {
        false
      } else {
        !room.rocks.get().exists(rock => newRock.intersects(rock))
      }
    }

    def canMoveRight(room: TallRoom): Boolean = {
      val newRock = moveRight
      if (newRock.location.col + newRock.shape.width > room.width) {
        false
      } else {
        !room.rocks.get().exists(rock => newRock.intersects(rock))
      }
    }

    def canMoveDown(room: TallRoom): Boolean = {
      val newRock = moveDown
      if (newRock.location.row - (newRock.shape.height - 1) < 0) {
        false
      } else {
        !room.rocks.get().exists(rock => newRock.intersects(rock))
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
    val sep = 100
    val jets = parseInput("input")
    val room = new TallRoom(rocks = new PartitionBuffer(sep, map = new mutable.HashMap()))

    var moveNum = 0

    var currentShapeIndex = 0

    (0 until 2022).foreach { rockNum =>
      println(s"Rock number: $rockNum")

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
  }
}
