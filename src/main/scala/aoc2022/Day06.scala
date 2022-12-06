package aoc2022

object Day06 {
  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day06/input")
    // _ ( _ _ _ _)
    val packet = contents
      .sliding(4)
      .zipWithIndex
      .find { case (window, _) =>
        window.toCharArray.toSet.size == 4
      }
      .get

    print(packet._2 + 4)
  }
}

object Day06Part2 {
  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day06/input")
    val packet = contents
      .sliding(14)
      .zipWithIndex
      .find { case (window, _) =>
        window.toCharArray.toSet.size == 14
      }
      .get

    print(packet._2 + 14)
  }
}
