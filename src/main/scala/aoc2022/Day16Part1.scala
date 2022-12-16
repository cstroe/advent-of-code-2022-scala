package aoc2022

import scala.collection.mutable

object Day16Part1 {
  case class Valve(name: String, flowRate: Long, connectedTo: List[String]) {
    override def toString: String = s"""$name --- $flowRate ----> ${connectedTo.mkString(",")}"""
  }

  sealed trait ValveStatus
  case object Closed extends ValveStatus {
    override def toString: String = "c"
  }
  case object Open extends ValveStatus {
    override def toString: String = "o"
  }

  case class Hop(valve: String, status: ValveStatus) {
    override def toString: String = s"$valve$status"
  }
  case class Path(hops: List[Hop], pressureReleased: Long)

  def parseInput(fileName: String): Map[String, Valve]  = {
    val lineParse =
      """Valve ([A-Z]+) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-Z]+[,A-Z ]*)""".r

    readFileToLines(s"src/main/resources/day16/$fileName").map {
      case lineParse(valveName, flowRate, connectedTo) =>
        val leadTo = connectedTo.split(",").map(_.trim).toList
        Valve(valveName, flowRate.toLong, leadTo)
      case other => throw new RuntimeException(s"could not parse: $other")
    }.map(v => (v.name, v)).toMap
  }

  def expandPath(path: Path, valves: Map[String, Valve], minute: Int, numMinutes: Int): List[Path] = {
    val openValves = path.hops.filter(_.status == Open).map(_.valve).toSet
    val currentLocation = path.hops.last.valve

    val selfHops: List[Hop] = if (!openValves.contains(currentLocation)) {
      List(Hop(currentLocation, Open))
    } else { List.empty }

    (selfHops ++ valves(currentLocation).connectedTo.map { adjacentValve =>
      if (openValves.contains(adjacentValve)) {
        Hop(adjacentValve, Open)
      } else {
        Hop(adjacentValve, Closed)
      }
    }).map { possibleHop =>
      if (possibleHop.status == Open && !openValves.contains(possibleHop.valve)) {
        val valveFlowRate = valves(possibleHop.valve).flowRate
        val pressureRelease = (numMinutes - minute) * valveFlowRate
        Path(path.hops :+ possibleHop, path.pressureReleased + pressureRelease)
      } else {
        Path(path.hops :+ possibleHop, path.pressureReleased)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val valves = parseInput("input")
    val startingPath = Path(List(Hop("AA", Closed)), 0)
    val paths = (1 to 30).foldLeft(List(startingPath)) { case (paths, minute) =>
      println(s"== Minute $minute ==")
      println(s"Existing paths: ${paths.size}")

      val newPaths = paths.flatMap { expandPath(_, valves, minute, 30) }
      println(s"New paths: ${paths.size}")

      newPaths.sortBy(_.pressureReleased*(-1)).take(200_000)
    }

    println(s"Found ${paths.size} paths")

    paths.sortBy(_.pressureReleased*(-1)).take(10).foreach(println(_))
  }
}
