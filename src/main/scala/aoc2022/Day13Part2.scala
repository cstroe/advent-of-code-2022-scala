package aoc2022

import aoc2022.Day13._

object Day13Part2 {
  def main(args: Array[String]): Unit = {
    val markerPackets = List("[[2]]", "[[6]]")
    val output: Int =
      (readFileToLines("src/main/resources/day13/input") ++ markerPackets)
        .filterNot(_.trim.isBlank)
        .sortInPlaceWith { case (left, right) =>
          compare(left, right) match {
            case Smaller | LeftOutOfItems => true
            case _                        => false
          }
        }
        .zipWithIndex
        .map { case (term, i) => (term, i + 1) }
        .filter { case (term, _) => markerPackets.contains(term) }
        .map(_._2)
        .product

    assert(output == 23111)
    println(output)
  }
}
