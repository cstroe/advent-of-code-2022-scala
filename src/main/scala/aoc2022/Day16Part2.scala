package aoc2022

object Day16Part2 {
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

  case class Path(yourHops: List[Hop], elephantHops: List[Hop], pressureReleased: Long)

  def parseInput(fileName: String): Map[String, Valve] = {
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
    val openValves = (
      path.yourHops.filter(_.status == Open) ++
        path.elephantHops.filter(_.status == Open)
      ).map(_.valve).toSet
    val yourLocation = path.yourHops.last.valve
    val elephantLocation = path.elephantHops.last.valve

    val selfHops: List[Hop] = if (!openValves.contains(yourLocation)) {
      List(Hop(yourLocation, Open))
    } else {
      List.empty
    }

    val elephantSelfHops: List[Hop] = if (!openValves.contains(elephantLocation)) {
      List(Hop(elephantLocation, Open))
    } else {
      List.empty
    }

    val yourNewHopsWithPressure = (selfHops ++ valves(yourLocation).connectedTo.map { adjacentValve =>
      if (openValves.contains(adjacentValve)) {
        Hop(adjacentValve, Open)
      } else {
        Hop(adjacentValve, Closed)
      }
    }).map { possibleHop =>
      if (possibleHop.status == Open && !openValves.contains(possibleHop.valve)) {
        val valveFlowRate = valves(possibleHop.valve).flowRate
        val pressureRelease = (numMinutes - minute) * valveFlowRate
        (path.yourHops :+ possibleHop, pressureRelease)
      } else {
        (path.yourHops :+ possibleHop, 0L)
      }
    }

    val elephantHopsWithPressure: List[(List[Hop], Long)] = (elephantSelfHops ++ valves(elephantLocation).connectedTo.map { adjacentValve =>
      if (openValves.contains(adjacentValve)) {
        Hop(adjacentValve, Open)
      } else {
        Hop(adjacentValve, Closed)
      }
    }).map { possibleHop =>
      if (possibleHop.status == Open && !openValves.contains(possibleHop.valve)) {
        val valveFlowRate = valves(possibleHop.valve).flowRate
        val pressureRelease = (numMinutes - minute) * valveFlowRate
        (path.elephantHops :+ possibleHop, pressureRelease)
      } else {
        (path.elephantHops :+ possibleHop, 0L)
      }
    }

    val newHops: List[(List[Hop], List[Hop], Long)] = for {
      yourHop <- yourNewHopsWithPressure
      elephantHop <- elephantHopsWithPressure
    } yield { (
      yourHop._1,
      elephantHop._1,
      yourHop._2 +
        elephantHop._2
    ) }

    newHops
      .filter { case (yourPath, elephantPath, _) =>
        val yourLastHop = yourPath.last
        val elephantLastHop = elephantPath.last
        if (yourLastHop.valve == elephantLastHop.valve && yourLastHop.status == Open && elephantLastHop.status == Open) {
          // don't open the same valve at the same time
          false
        } else { true }
      }.map { case (yourHops, elephantHops, newPressure) =>
      Path(yourHops, elephantHops, path.pressureReleased + newPressure)
    }
  }

  def main(args: Array[String]): Unit = {
    val valves = parseInput("input")
    val startingPath = Path(yourHops = List(Hop("AA", Closed)), elephantHops = List(Hop("AA", Closed)), 0)
    val numMinutes = 26
    val paths = (1 to numMinutes).foldLeft(List(startingPath)) { case (paths, minute) =>
      println(s"== Minute $minute ==")
      println(s"Existing paths: ${paths.size}")

      val newPaths = paths.flatMap {
        expandPath(_, valves, minute, numMinutes)
      }
      println(s"New paths: ${paths.size}")

      newPaths.sortBy(_.pressureReleased * (-1)).take(200_000)
    }

    println(s"Found ${paths.size} paths")

    paths.sortBy(_.pressureReleased * (-1)).take(10).foreach(println(_))
  }
}
