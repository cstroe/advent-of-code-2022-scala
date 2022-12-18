package aoc2022

import aoc2022.Day15Part1.{Point, manhattanDistance}

object Day15Part2 {
  case class Coefficients(A: Long, B: Long, C: Long)
  case class Line(start: Point, end: Point) {
    val coefficients: Coefficients = {
      // equation of a line:
      // y = (rise/run)x + b
      // run*y = rise*x + b
      // 0 = rise*x - run*y + b
      // b = run*y - rise*x
      val rise = end.y - start.y
      val run = end.x - start.x
      val b = run * start.y - rise * start.x

      // move it to form Ax + By = C
      // b = run*y - rise*x
      // -b = rise*x - run*y
      val A = rise
      val B = -1 * run
      val C = -1 * b
      Coefficients(A, B, C)
    }
    def intersectionPoint(line: Line): Option[Point] = {
      // point of intersection (https://www.topcoder.com/thrive/articles/Geometry%20Concepts%20part%202:%20%20Line%20Intersection%20and%20its%20Applications)

      val A1 = coefficients.A
      val A2 = line.coefficients.A
      val B1 = coefficients.B
      val B2 = line.coefficients.B
      val C1 = coefficients.C
      val C2 = line.coefficients.C

      val det = A1 * B2 - A2 * B1
      if (det == 0) {
        None
      } else {
        val x = (B2 * C1 - B1 * C2) / det
        val y = (A1 * C2 - A2 * C1) / det
        Option(Point(x, y))
      }
    }
  }
  case class Area(point: Point, distance: Long) {
    val lines: List[Line] = List(
      Line(
        Point(point.x - distance, point.y),
        Point(point.x, point.y - distance)
      ),
      Line(
        Point(point.x, point.y - distance),
        Point(point.x + distance, point.y)
      ),
      Line(
        Point(point.x + distance, point.y),
        Point(point.x, point.y + distance)
      ),
      Line(
        Point(point.x, point.y + distance),
        Point(point.x - distance, point.y)
      )
    )
    def intersectionPoints(area: Area): List[Point] = {
      val points = for {
        l1 <- lines
        l2 <- area.lines
      } yield l1.intersectionPoint(l2)
      points.flatten
    }

    def contains(test: Point): Boolean = isWithinPoint(test, point, distance)
  }

  def isWithinPoint(test: Point, sensor: Point, distance: Long): Boolean = {
    manhattanDistance(test, sensor) <= distance
  }

  def main(args: Array[String]): Unit = {
    val searchTo = 4000000
    val lineParse =
      """Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)""".r

    println("Reading data")
    val data = readFileToLines("src/main/resources/day15/input")
      .map {
        case lineParse(sx, sy, bx, by) =>
          (Point(sx.toLong, sy.toLong), Point(bx.toLong, by.toLong))
        case _ => throw new RuntimeException("invalid parse")
      }
      .map { case (sensor, beacon) =>
        (sensor, beacon, manhattanDistance(sensor, beacon))
      }
      .toList

    val areas = data.map { case (sensor, _, distance) =>
      Area(sensor, distance)
    }
    val numAreas = areas.size

    println(s"Found $numAreas areas")
    println(s"Computing intersection points")
    val intersectionPoints: List[Point] = (for {
      a1 <- areas
      a2 <- areas
    } yield a1.intersectionPoints(a2)).flatten

    println(s"Found ${intersectionPoints.size} intersection points")

    val pointsOfInterest = intersectionPoints
      .flatMap { point => point.pointsAround() }
      .filter { point =>
        point.x >= 0 && point.x <= searchTo && point.y >= 0 && point.y <= searchTo
      }
      .toSet

    println(s"Found ${pointsOfInterest.size} points of interest")

    val pointsLeft = pointsOfInterest.filter { point =>
      val foundIn = areas.map { area =>
        if (area.contains(point)) { 1 }
        else { 0 }
      }.sum
      foundIn == 0
    }

    println(s"Found ${pointsLeft.size} points:")
    pointsLeft.zipWithIndex.foreach(println(_))

    val finalPoint = pointsLeft.toList.head

    println(s"Tuning frequency = ${finalPoint.x * 4000000 + finalPoint.y}")
  }
}
