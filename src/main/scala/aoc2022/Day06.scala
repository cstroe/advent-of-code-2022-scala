package aoc2022

object Day06 {
  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day06/input")
    // _ ( _ _ _ _)
    val (window, startOfWindow) = contents
      .sliding(4)
      .zipWithIndex
      .find { case (window, _) =>
        window.toCharArray.toSet.size == 4
      }
      .get

    print(startOfWindow + window.length)
  }
}

object Day06Part2 {
  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day06/input")
    val (window, startOfWindow) = contents
      .sliding(14)
      .zipWithIndex
      .find { case (window, _) =>
        window.toCharArray.toSet.size == 14
      }
      .get

    print(startOfWindow + window.length)
  }
}
