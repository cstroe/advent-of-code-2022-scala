package aoc2022

import aoc2022.Day13._

object Day13Part2 {

  def main(args: Array[String]): Unit = {
    val contents =
      readFileToLines("src/main/resources/day13/input") ++ List(
        "[[2]]",
        "[[6]]"
      )

    val sorted: Array[String] =
      contents
        .filterNot(_.trim.isBlank)
        .sortInPlaceWith { case (left, right) =>
          compare(left, right) match {
            case Smaller | LeftOutOfItems => true
            case _                        => false
          }
        }
        .toArray

    val indices = sorted.zipWithIndex
      .map { case (term, index) => (term, index + 1) }
      .filter { case (term, _) =>
        term == "[[2]]" || term == "[[6]]"
      }
      .map(_._2)
      .toList

    val output = indices(0) * indices(1)
    assert(output == 23111)
    println(output)
  }
}
