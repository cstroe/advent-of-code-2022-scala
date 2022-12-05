package aoc2022

object Day04 {
  case class Assignment(from: Int, to: Int) {
    def contains(other: Assignment): Boolean = {
      other.from >= from && other.to <= to
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day04/input")
    val count = contents
      .map { line =>
        val pair = line.split(",")

        val ass1 = pair(0).split("-")
        val ass2 = pair(1).split("-")
        (
          Assignment(ass1(0).toInt, ass1(1).toInt),
          Assignment(ass2(0).toInt, ass2(1).toInt)
        )
      }
      .map { pair =>
        if (pair._1.contains(pair._2) || pair._2.contains(pair._1)) {
          1
        } else {
          0
        }
      }
      .sum

    println(count)
  }

}

object Day04Part2 {
  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day04/input")
    val count = contents
      .map { line =>
        val pair = line.split(",")

        val ass1 = pair(0).split("-")
        val ass2 = pair(1).split("-")
        (
          Range.inclusive(ass1(0).toInt, ass1(1).toInt),
          Range.inclusive(ass2(0).toInt, ass2(1).toInt)
        )
      }
      .map { pair =>
        if (pair._1.toSet.intersect(pair._2.toSet).nonEmpty) {
          1
        } else {
          0
        }
      }
      .sum

    println(count)
  }

}
