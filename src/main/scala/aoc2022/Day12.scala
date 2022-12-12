package aoc2022

import scala.collection.mutable

/** DFS took forever on the main input, couldn't get an answer. Switched to BFS
  * after looking up an example. I need to understand what I messed up with DFS.
  */
object Day12 {
  case class TopoPoint(row: Int, col: Int, value: Char)

  class TopoMap(map: Array[Array[TopoPoint]]) {
    def neighbors(point: TopoPoint): Seq[TopoPoint] = {
      List((-1, 0), (1, 0), (0, -1), (0, 1))
        .map { case (rd, cd) =>
          (point.row + rd, point.col + cd)
        }
        .filter { case (r, c) =>
          r >= 0 && r < map.length && c >= 0 && c < map(0).length
        }
        .map { case (r, c) => map(r)(c) }
    }
  }

  def parseInput(lines: Array[String]): (TopoMap, TopoPoint, TopoPoint) = {
    var start: TopoPoint = TopoPoint(0, 0, 0)
    var end: TopoPoint = TopoPoint(0, 0, 0)

    val topmap: Array[Array[TopoPoint]] = lines.zipWithIndex.map {
      case (line, row) =>
        line.toCharArray.zipWithIndex.map { case (char, col) =>
          char match {
            case 'S' =>
              val point = TopoPoint(row, col, 'a')
              start = point
              point
            case 'E' =>
              val point = TopoPoint(row, col, 'z')
              end = point
              point
            case c => TopoPoint(row, col, c)
          }
        }
    }

    val map = new TopoMap(topmap)

    (map, start, end)
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day12/input")

    val (map, start, end) = parseInput(contents)

    val queue = new mutable.Queue[(TopoPoint, Int)]()
    queue.enqueue((start, 0))

    val visited = mutable.HashSet(start)

    var run = true
    var shortest = Int.MaxValue
    while (run && queue.nonEmpty) {
      val (currentPos, currentLength) = queue.dequeue()
      if (currentPos == end) {
        shortest = currentLength
        run = false
      } else {
        map.neighbors(currentPos).foreach { neighbor =>
          if (
            neighbor.value - currentPos.value <= 1 && !visited
              .contains(neighbor)
          ) {
            queue.enqueue((neighbor, currentLength + 1))
            visited.add(neighbor)
          }
        }
      }
    }

    println("Day12")
    println(shortest)
  }
}

object Day12Part2 {
  case class TopoPoint(row: Int, col: Int, value: Char)

  class TopoMap(val map: Array[Array[TopoPoint]]) {
    def neighbors(point: TopoPoint): Seq[TopoPoint] = {
      List((-1, 0), (1, 0), (0, -1), (0, 1))
        .map { case (rd, cd) =>
          (point.row + rd, point.col + cd)
        }
        .filter { case (r, c) =>
          r >= 0 && r < map.length && c >= 0 && c < map(0).length
        }
        .map { case (r, c) => map(r)(c) }
    }
  }

  def parseInput(lines: Array[String]): (TopoMap, TopoPoint, TopoPoint) = {
    var start: TopoPoint = TopoPoint(0, 0, 0)
    var end: TopoPoint = TopoPoint(0, 0, 0)

    val topmap: Array[Array[TopoPoint]] = lines.zipWithIndex.map {
      case (line, row) =>
        line.toCharArray.zipWithIndex.map { case (char, col) =>
          char match {
            case 'S' =>
              val point = TopoPoint(row, col, 'a')
              start = point
              point
            case 'E' =>
              val point = TopoPoint(row, col, 'z')
              end = point
              point
            case c => TopoPoint(row, col, c)
          }
        }
    }

    val map = new TopoMap(topmap)

    (map, start, end)
  }

  def findShortest(start: TopoPoint, end: TopoPoint, map: TopoMap): Int = {
    val queue = new mutable.Queue[(TopoPoint, Int)]()
    queue.enqueue((start, 0))

    val visited = mutable.HashSet(start)

    var run = true
    var shortest = Int.MaxValue
    while (run && queue.nonEmpty) {
      val (currentPos, currentLength) = queue.dequeue()
      if (currentPos == end) {
        shortest = currentLength
        run = false
      } else {
        map.neighbors(currentPos).foreach { neighbor =>
          if (
            neighbor.value - currentPos.value <= 1 && !visited
              .contains(neighbor)
          ) {
            queue.enqueue((neighbor, currentLength + 1))
            visited.add(neighbor)
          }
        }
      }
    }

    shortest
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day12/input")
    val (map, _, end) = parseInput(contents)
    val starts = map.map.flatten.filter(_.value == 'a')
    println(starts.map(findShortest(_, end, map)).min)
  }
}
