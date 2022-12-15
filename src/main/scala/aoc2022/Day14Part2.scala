package aoc2022

import aoc2022.Day14Part1.Point

import scala.annotation.tailrec
import scala.collection.mutable

/** This implementation is slow, it should be sped up
  */
object Day14Part2 {
  sealed trait Line {
    def intersects(point: Point): Boolean
  }

  case class FiniteLine(start: Point, end: Point) extends Line {
    private val lowY = Math.min(start.y, end.y)
    private val highY = Math.max(start.y, end.y)
    private val lowX = Math.min(start.x, end.x)
    private val highX = Math.max(start.x, end.x)
    override def intersects(point: Point): Boolean = {
      point.x >= lowX && point.x <= highX && point.y >= lowY && point.y <= highY
    }
  }

  case class Floor(y: Int) extends Line {
    override def intersects(point: Point): Boolean = {
      point.y == y
    }
  }

  def isOccupied(
      point: Point,
      rocks: List[Line],
      grains: mutable.Set[Point]
  ): Boolean = {
    val hitRock = rocks.exists(_.intersects(point))
    val hitGrain = grains.contains(point)
    hitRock || hitGrain
  }

  @tailrec
  def comeToRest(
      grainStart: Point,
      rocks: List[Line],
      grains: mutable.Set[Point]
  ): Option[Point] = {
    if (!isOccupied(grainStart.down(), rocks, grains)) {
      comeToRest(grainStart.down(), rocks, grains)
    } else if (!isOccupied(grainStart.left(), rocks, grains)) {
      comeToRest(grainStart.left(), rocks, grains)
    } else if (!isOccupied(grainStart.right(), rocks, grains)) {
      comeToRest(grainStart.right(), rocks, grains)
    } else {
      println(s"Came to rest at: $grainStart")
      Option(grainStart)
    }
  }

  def main(args: Array[String]): Unit = {
    val rocks: List[FiniteLine] =
      readFileToLines("src/main/resources/day14/input")
        .map { line =>
          line
            .split(" -> ")
            .map { coord =>
              val splits = coord.split(",")
              Point(splits(0).toInt, splits(1).toInt)
            }
            .toList
        }
        .flatMap { rocks =>
          rocks
            .sliding(2)
            .map { g =>
              FiniteLine(g(0), g(1))
            }
            .toList
        }
        .toList

    val highestY = rocks.flatMap(l => List(l.start.y, l.end.y)).max

    val rocksWithFloor = rocks.map(_.asInstanceOf[Line]) :+ Floor(highestY + 2)

    var filledUp = false
    val grainsOfSand = mutable.HashSet[Point]()
    while (!filledUp) {
      println(s"Num grains: ${grainsOfSand.size}")
      if (isOccupied(Point(500, 0), rocksWithFloor, grainsOfSand)) {
        filledUp = true
      } else {
        comeToRest(Point(500, 0), rocksWithFloor, grainsOfSand) match {
          case None =>
          case Some(grainCameToRest) =>
            grainsOfSand.add(grainCameToRest)
        }
      }
    }

    assert(grainsOfSand.size == 27324)
    println(s"Grains of stand that came to rest: ${grainsOfSand.size}")
  }
}
