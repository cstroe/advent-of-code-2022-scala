package aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day14Part1 {
  // creating new points for this is slow
  case class Point(x: Int, y: Int) {
    def down(): Point = Point(x, y+1)
    def left(): Point = Point(x-1, y+1)
    def right(): Point = Point(x+1, y+1)
  }
  case class Line(start: Point, end: Point) {
    val points: Set[Point] = {
      if (start.x == end.x) {
        val lowY = Math.min(start.y, end.y)
        val highY = Math.max(start.y, end.y)
        (lowY to highY).map { y => Point(start.x, y) }.toSet
      } else {
        val lowX = Math.min(start.x, end.x)
        val highX = Math.max(start.x, end.x)
        (lowX to highX).map { x => Point(x, start.y) }.toSet
      }
    }
  }

  def isOccupied(point: Point, rocks: List[Line], grains: Set[Point]): Boolean = {
    val hitRock = rocks.exists(_.points.contains(point))
    val hitGrain = grains.contains(point)

    //    if (hitRock) { println(s"Grain at ${point} hit a rock.") }
    //    if (hitGrain) { println(s"Grain at ${point} hit another grain.")}
    hitRock || hitGrain
  }

  @tailrec
  def comeToRest(grainStart: Point, rocks: List[Line], grains: Set[Point], highestY: Int): Option[Point] = {
    //println(grainStart)
    if (grainStart.y > highestY) {
      None
    } else {
      // try to move down
      if (!isOccupied(grainStart.down(), rocks, grains)) {
        comeToRest(grainStart.down(), rocks, grains, highestY)
      } else if (!isOccupied(grainStart.left(), rocks, grains)) {
        comeToRest(grainStart.left(), rocks, grains, highestY)
      } else if (!isOccupied(grainStart.right(), rocks, grains)) {
        comeToRest(grainStart.right(), rocks, grains, highestY)
      } else {
        println(s"Came to rest at: $grainStart")
        Option(grainStart)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val rocks: List[Line] =
      readFileToLines("src/main/resources/day14/input")
        .map { line =>
          line.split(" -> ").map { coord =>
            val splits = coord.split(",")
            Point(splits(0).toInt, splits(1).toInt)
          }.toList
        }.flatMap { rocks =>
        rocks.sliding(2).map {
          g => Line(g(0), g(1))
        }.toList
      }.toList

    val highestY = rocks.flatMap(l => List(l.start.y, l.end.y)).max

    var fellIntoAbyss = false
    val grainsOfSand = mutable.ArrayBuffer[Point]()
    while (!fellIntoAbyss) {
      println(s"Num grains: ${grainsOfSand.length}")
      comeToRest(Point(500, 0), rocks, grainsOfSand.toSet, highestY) match {
        case None => fellIntoAbyss = true
        case Some(grainCameToRest) =>
          grainsOfSand.append(grainCameToRest)
      }
    }

    println(s"Grains of stand that came to rest: ${grainsOfSand.length}")
  }
}
