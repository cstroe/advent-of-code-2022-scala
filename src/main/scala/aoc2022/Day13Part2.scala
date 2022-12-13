package aoc2022

import aoc2022.Day13._

object Day13Part2 {
  def main(args: Array[String]): Unit = {
    val output: Int =
      (readFileToLines("src/main/resources/day13/input") ++ List(
        "[[2]]",
        "[[6]]"
      )).filterNot(_.trim.isBlank)
        .sortInPlaceWith { case (left, right) =>
          compare(left, right) match {
            case Smaller | LeftOutOfItems => true
            case _                        => false
          }
        }
        .zipWithIndex
        .map { case (term, index) => (term, index + 1) }
        .filter { case (term, _) =>
          term == "[[2]]" || term == "[[6]]"
        }
        .map(_._2)
        .product

    assert(output == 23111)
    println(output)
  }
}
