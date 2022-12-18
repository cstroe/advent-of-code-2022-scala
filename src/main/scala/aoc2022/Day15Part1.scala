package aoc2022

object Day15Part1 {
  case class Point(x: Long, y: Long) {
    def pointsAround(): List[Point] = {
      List(
        Point(x, y),
        Point(x - 1, y),
        Point(x - 1, y - 1),
        Point(x, y - 1),
        Point(x + 1, y - 1),
        Point(x + 1, y),
        Point(x + 1, y + 1),
        Point(x, y + 1),
        Point(x - 1, y + 1)
      )
    }
  }

  def manhattanDistance(start: Point, other: Point): Long = {
    Math.abs(start.x - other.x) + Math.abs(start.y - other.y)
  }

  def isWithinPoint(test: Point, sensor: Point, distance: Long): Boolean = {
    manhattanDistance(test, sensor) <= distance
  }

  def main(args: Array[String]): Unit = {
    val row = 10L
    val lineParse =
      """Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)""".r

    val data = readFileToLines("src/main/resources/day15/testinput")
      .map {
        case lineParse(sx, sy, bx, by) =>
          (Point(sx.toLong, sy.toLong), Point(bx.toLong, by.toLong))
        case _ => throw new RuntimeException("invalid parse")
      }
      .map { case (sensor, beacon) =>
        (sensor, beacon, manhattanDistance(sensor, beacon))
      }
      .toList

    val sensorData = data.map { case (sensor, _, distance) =>
      (sensor, distance)
    }
    val minX = sensorData.map { case (sensor, distance) =>
      sensor.x - distance
    }.min
    val maxX = sensorData.map { case (sensor, distance) =>
      sensor.y + distance
    }.max

    println(s"minX = $minX, maxX = $maxX")

    val rowsWhereCannotBe: Int = (minX to maxX)
      .map(Point(_, row))
      .map { test =>
        if (test.x % 10000 == 0) { println(s"Testing $test") }
        if (
          sensorData.exists { case (sensor, distance) =>
            isWithinPoint(test, sensor, distance)
          }
        ) {
          // println(s"testing $test -> true")
          1
        } else {
          // println(s"testing $test -> false")
          0
        }
      }
      .sum

    println(rowsWhereCannotBe - 1)
  }
}
